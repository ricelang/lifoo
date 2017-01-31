(defpackage lifoo
  (:export define-lisp-ops define-lisp-word define-word do-lifoo
           lifoo-compile lifoo-define
           lifoo-eval lifoo-init lifoo-parse lifoo-pop lifoo-push
           lifoo-read lifoo-repl lifoo-stack
           lifoo-undefine lifoo-word lifoo-words make-lifoo)
  (:use cl cl4l-compare cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lisp-word (name (exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,exec ',name (lambda () ,@body)))

(defmacro define-lisp-ops ((exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) (exec)
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
       (lifoo-eval ,_exec '(,@body))
       (lifoo-pop ,_exec))))

(defstruct (lifoo)
  stack (words (make-hash-table :test 'eq)))

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
                       (cons `(funcall ,(lifoo-word exec e))
                             acc)))
                 ((functionp e)
                  (rec (rest ex)
                       (cons `(funcall ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))))
             (nreverse acc))))
    (rec (list! expr) nil)))

(defun lifoo-read (&key (in *standard-input*))
  (let ((more?) (expr))
    (do-while ((setf more? (read in nil)))
      (push more? expr))
    (nreverse expr)))

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
  (setf (gethash (keyword! id) (lifoo-words exec)) fn))

(defun lifoo-undefine (exec id)
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (lifoo-words exec)))

(defun lifoo-word (exec id)
  "Returns word named ID from EXEC or signals error if missing"  
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

(defun lifoo-repl (&key (exec (lifoo-init (make-lifoo)))
                        (in *standard-input*)
                        (prompt "lifoo>")
                        (out *standard-output*))
  (tagbody
   start
     (format out "~%~a " prompt)
     (when-let (line (read-line in nil))
       (unless (string= "" line)
         (with-input-from-string (in line)
           (lifoo-eval exec (lifoo-read :in in))
           (format out "~a~%" (lifoo-pop exec)))
         (go start)))))

(defun lifoo-init (exec)
  "Initializes EXEC with built-in words"

  ;; Define binary ops
  (define-lisp-ops (exec) + - * / = /= < > cons)

  ;; Replaces $1 and $2 with result of comparing $2 to $1
  (define-lisp-word :cmp (exec)
    (let ((rhs (lifoo-pop exec))
          (lhs (lifoo-pop exec)))
      (lifoo-push exec (compare lhs rhs))))

  ;; Derived comparison ops
  (define-word :eq? (exec) cmp 0 =)
  (define-word :neq? (exec) cmp 0 /=)
  (define-word :lt? (exec) cmp -1 =)
  (define-word :gt? (exec) cmp 1 =)
  (define-word :lte? (exec) cmp 1 <)
  (define-word :gte? (exec) cmp -1 >)

  ;; Pops and repeats body in $2 x $1
  (define-lisp-word :do-times (exec)
    (let ((reps (lifoo-pop exec))
          (body (lifoo-parse exec (lifoo-pop exec))))
      (dotimes (i reps)
        (lifoo-push exec i)
        (eval `(progn ,@body)))))

  ;; Drops $1 from stack
  (define-lisp-word :drop (exec)
    (lifoo-pop exec))

  ;; Pushes $1 on stack
  (define-lisp-word :dup (exec)
    (lifoo-push exec (first (lifoo-stack exec))))
  
  ;; Replaces $1 with result of evaluating it
  (define-lisp-word :eval (exec)
    (lifoo-push exec
                (lifoo-eval exec
                            (lifoo-pop exec))))

  ;; Replaces $1 with result of compiling it
  (define-lisp-word :compile (exec)
    (lifoo-push exec
                (lifoo-compile exec
                               (lifoo-pop exec))))

  ;; Replaces $1 with first element of list
  (define-lisp-word :first (exec)
    (lifoo-push exec (first (lifoo-pop exec))))

  ;; Replaces $1 (arguments) and $2 (format) with formatted output
  (define-lisp-word :format (exec)
    (let ((args (lifoo-pop exec))
          (fmt (lifoo-pop exec)))
      (lifoo-push exec (apply #'format nil fmt args))))

  ;; Clears and pushes stack
  (define-lisp-word :list (exec)
    (let ((lst (lifoo-stack exec)))
      (setf (lifoo-stack exec) nil)
      (lifoo-push exec (nreverse lst))))

  ;; Prints line ending
  (define-lisp-word :ln (exec)
    (terpri))

  ;; Replaces $1 and $2 with results of mapping $1 over $2
  (define-lisp-word :map (exec)
    (let ((fn (lifoo-compile exec (lifoo-pop exec)))
          (lst (lifoo-pop exec)))
      (lifoo-push exec (mapcar (lambda (it)
                                 (lifoo-push exec it)
                                 (funcall fn)
                                 (lifoo-pop exec))
                               lst))))

  ;; Replaces $1 with T if NIL, otherwise NIL
  (define-lisp-word :nil? (exec)
    (lifoo-push exec
                (null (lifoo-eval exec
                                  (lifoo-pop exec)))))

  ;; Pops and prints $1
  (define-lisp-word :print (exec)
    (princ (lifoo-pop exec)))

  ;; Pops item from list in $1 and pushes it on stack
  (define-lisp-word :pop (exec)
    (let ((it (pop (first (lifoo-stack exec)))))
      (lifoo-push exec it)))

  ;; Pops $1 from stack and pushes it on list in $2
  (define-lisp-word :push (exec)
    (let ((it (lifoo-pop exec)))
      (push it (first (lifoo-stack exec)))))

  ;; Replaces $1 with rest of list
  (define-lisp-word :rest (exec)
    (lifoo-push exec
                (rest (lifoo-pop exec))))

  ;; Replaces $1 with reversed list
  (define-lisp-word :reverse (exec)
    (lifoo-push exec (reverse (lifoo-pop exec))))

  ;; Sleeps for $1 seconds
  (define-lisp-word :sleep (exec)
    (sleep (lifoo-pop exec)))

  ;; Replaces $1 with string representation
  (define-lisp-word :string (exec)
    (let ((val (lifoo-pop exec)))
      (lifoo-push exec (if (listp val)
                           (apply #'string! val)
                           (string! val)))))

  ;; Replaces $1 with results of converting it to a symbol
  (define-lisp-word :symbol (exec)
    (lifoo-push exec (keyword! (lifoo-pop exec))))
  
  ;; Swaps $1 and $2
  (define-lisp-word :swap (exec)
    (push (lifoo-pop exec)
          (rest (lifoo-stack exec))))

  ;; Replaces $1 and $2 with results of evaluating $2 if $1,
  ;; otherwise NIL
  (define-lisp-word :when (exec)
    (let ((cnd (lifoo-pop exec))
          (res (lifoo-pop exec)))
      (lifoo-eval exec cnd)
      (if (lifoo-pop exec)
          (lifoo-eval exec res)
          (lifoo-push exec nil))))

  ;; Replaces $1 and $2 with results of evaluating $2 if $1 is NIL,
  ;; otherwise NIL
  (define-word :unless (exec) nil? when)

  ;; Replaces $1 with the word it represents
  (define-lisp-word :word (exec)
    (let ((fn (lifoo-word exec (lifoo-pop exec))))
      (lifoo-push exec fn)))

  exec)
