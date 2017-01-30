(defpackage lifoo
  (:export define-lisp-ops define-lisp-word define-word do-lifoo
           lifoo-parse lifoo-compile lifoo-define lifoo-eval
           lifoo-pop lifoo-push make-lifoo lifoo-undefine)
  (:use cl cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lisp-word (name (exec) &body body)
  `(lifoo-define ,exec ',name (lambda () ,@body)))

(defmacro define-lisp-ops ((exec) &rest ops)
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,op (exec)
                      (let ((,_rhs (lifoo-pop ,exec))
                            (,_lhs (lifoo-pop ,exec)))
                        (lifoo-push ,exec (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (exec) &body body)
  `(lifoo-define ,exec ',name (lifoo-compile ,exec '(,@body))))

(defmacro do-lifoo ((&key exec) &body body)
  (with-symbols (_exec)
    `(let ((,_exec (or ,exec (make-lifoo))))
       (funcall (lifoo-compile ,_exec '(,@body)))
       (lifoo-pop ,_exec))))

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-exec))
  stack (words (make-hash-table :test 'eq)))

(defstruct (lifoo-word (:conc-name word-)
                       (:constructor make-word))
  id fn)

(defun make-lifoo ()
  (let ((exec (make-exec)))
    (define-lisp-ops (exec) + - * / = /= < >)

    (define-lisp-word cmp (exec)
      (let ((rhs (lifoo-pop exec))
            (lhs (lifoo-pop exec)))
        (lifoo-push exec (compare lhs rhs))))

    (define-lisp-word nil? (exec)
      (lifoo-push exec (null (lifoo-eval exec (lifoo-pop exec)))))
    
    (define-lisp-word drop (exec)
      (lifoo-pop exec))

    (define-lisp-word dup (exec)
      (lifoo-push exec (first (stack exec))))

    (define-lisp-word first (exec)
      (lifoo-push exec (first (lifoo-pop exec))))

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
      (lifoo-push exec (rest (lifoo-pop exec))))

    (define-lisp-word swap (exec)
      (push (lifoo-pop exec) (rest (stack exec))))

    (define-lisp-word when (exec)
      (let ((cnd (lifoo-pop exec))
            (res (lifoo-pop exec)))
        (lifoo-eval exec cnd)
        (when (lifoo-pop exec)
          (lifoo-eval exec res))))

    (define-word eq? (exec) cmp 0 =)
    (define-word neq? (exec) cmp 0 /=)
    (define-word lt? (exec) cmp -1 =)
    (define-word gt? (exec) cmp 1 =)
    (define-word lte? (exec) cmp 1 <)
    (define-word gte? (exec) cmp -1 >)
    
    (define-word unless (exec) nil? when)

    exec))

(defun lifoo-parse (exec expr)
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
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))))
             (nreverse acc))))
    (rec (list! expr) nil)))

(defun lifoo-eval (exec expr)
  (let ((pe (lifoo-parse exec expr)))
    (eval `(progn ,@pe))))

(defun lifoo-compile (exec expr)
  (eval `(lambda ()
           ,@(lifoo-parse exec expr))))

(defun lifoo-define (exec id fn)
  (setf (gethash (keyword! id) (words exec))
        (make-word :id id :fn fn)))

(defun lifoo-undefine (exec id)
  (remhash (keyword! id) (words exec)))

(defun lifoo-word (exec id)
  (gethash (keyword! id) (words exec)))

(defun lifoo-push (exec term)
  (push term (stack exec))
  term)

(defun lifoo-pop (exec)
  (pop (stack exec)))
