(defpackage lifoo
  (:export define-lisp-ops define-lisp-word define-word do-lifoo
           lifoo-parse lifoo-compile lifoo-define lifoo-eval
           lifoo-init lifoo-pop lifoo-push make-lifoo
           lifoo-undefine)
  (:use cl cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lisp-word (name (exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,exec ',name (lambda () ,@body)))

(defmacro define-lisp-ops ((exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,op (exec)
                      (let ((,_rhs (lifoo-pop ,exec))
                            (,_lhs (lifoo-pop ,exec)))
                        (lifoo-push ,exec (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ,exec ',name (lifoo-compile ,exec '(,@body))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  (with-symbols (_exec)
    `(let ((,_exec (or ,exec (lifoo-init (make-lifoo)))))
       (funcall (lifoo-compile ,_exec '(,@body)))
       (lifoo-pop ,_exec))))

(defstruct (lifoo)
  stack (words (make-hash-table :test 'eq)))

(defstruct (word)
  id fn)

(defun lifoo-parse (exec expr)
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((rec (ex acc)
         (if ex
             (let ((e (first ex)))
               (cond
                 ((consp e)
                  (rec (rest ex)
                       (cons `(lifoo-push ,exec '(,@e))
                             acc)))
                 ((null e)
                  (rec (rest ex) (cons `(lifoo-push ,exec nil)
                                       acc)))
                 ((eq e t)
                  (rec (rest ex) (cons `(lifoo-push ,exec t)
                                       acc)))
                 ((keywordp e)
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall
                               ,(word-fn (lifoo-word exec e)))
                             acc)))
                 ((functionp e)
                  (rec (rest ex)
                       (cons `(funcall ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))))
             (nreverse acc))))
    (rec (list! expr) nil)))

(defun lifoo-eval (exec expr)
  "Returns result of parsing and evaluating EXPR in EXEC"
  (let ((pe (lifoo-parse exec expr)))
    (eval `(progn ,@pe))))

(defun lifoo-compile (exec expr)
  "Returns result of parsing and compiling EXPR in EXEC"  
  (eval `(lambda ()
           ,@(lifoo-parse exec expr))))

(defun lifoo-define (exec id fn)
  "Defines word named ID in EXEC as FN"  
  (setf (gethash (keyword! id) (lifoo-words exec))
        (make-word :id id :fn fn)))

(defun lifoo-undefine (exec id)
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (lifoo-words exec)))

(defun lifoo-word (exec id)
  "Returns word named ID from EXEC"  
  (let ((word (gethash (keyword! id) (lifoo-words exec))))
    (unless word (error "missing word: ~a" id))
    word))

(defun lifoo-push (exec expr)
  "Pushes EXPR onto EXEC's stack"  
  (push expr (lifoo-stack exec))
  expr)

(defun lifoo-pop (exec)
  "Pops EXPR from EXEC's stack"  
  (pop (lifoo-stack exec)))

(defun lifoo-init (exec)
  "Initializes EXEC with built-in words"
  
  (define-lisp-ops (exec) + - * / = /= < > cons)

  (define-lisp-word cmp (exec)
    (let ((rhs (lifoo-pop exec))
          (lhs (lifoo-pop exec)))
      (lifoo-push exec (compare lhs rhs))))

  (define-lisp-word nil? (exec)
    (lifoo-push exec
                (null (lifoo-eval exec
                                  (lifoo-pop exec)))))
  
  (define-lisp-word drop (exec)
    (lifoo-pop exec))

  (define-lisp-word dup (exec)
    (lifoo-push exec (first (lifoo-stack exec))))

  (define-lisp-word eval (exec)
    (lifoo-push exec
                (lifoo-eval exec
                            (lifoo-pop exec))))

  (define-lisp-word compile (exec)
    (lifoo-push exec
                (lifoo-compile exec
                               (lifoo-pop exec))))

  (define-lisp-word first (exec)
    (lifoo-push exec (first (lifoo-pop exec))))

  (define-lisp-word format (exec)
    (let ((args (lifoo-pop exec))
          (fmt (lifoo-pop exec)))
      (lifoo-push exec (apply #'format nil fmt args))))

  (define-lisp-word intern (exec)
    (let ((str (string-upcase (lifoo-pop exec))))
      (lifoo-push exec (intern str :keyword))))
  
  (define-lisp-word list (exec)
    (let ((lst (lifoo-stack exec)))
      (setf (lifoo-stack exec) nil)
      (lifoo-push exec (nreverse lst))))

  (define-lisp-word ln (exec)
    (terpri))

  (define-lisp-word map (exec)
    (let ((fn (lifoo-compile exec (lifoo-pop exec)))
          (lst (lifoo-pop exec)))
      (lifoo-push exec (mapcar (lambda (it)
                                 (lifoo-push exec it)
                                 (funcall fn)
                                 (lifoo-pop exec))
                               lst))))

  (define-lisp-word print (exec)
    (princ (lifoo-pop exec)))

  (define-lisp-word rest (exec)
    (lifoo-push exec
                (rest (lifoo-pop exec))))

  (define-lisp-word swap (exec)
    (push (lifoo-pop exec)
          (rest (lifoo-stack exec))))

  (define-lisp-word when (exec)
    (let ((cnd (lifoo-pop exec))
          (res (lifoo-pop exec)))
      (lifoo-eval exec cnd)
      (when (lifoo-pop exec)
        (lifoo-eval exec res))))

  (define-lisp-word word (exec)
    (let ((w (lifoo-word exec (lifoo-pop exec))))
      (lifoo-push exec (funcall (word-fn w)))))

  (define-word eq? (exec) cmp 0 =)
  (define-word neq? (exec) cmp 0 /=)
  (define-word lt? (exec) cmp -1 =)
  (define-word gt? (exec) cmp 1 =)
  (define-word lte? (exec) cmp 1 <)
  (define-word gte? (exec) cmp -1 >)
  
  (define-word unless (exec) nil? when)

  exec)
