(defpackage lifoo
  (:use cl cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lisp-word (name (exec) &body body)
  `(lifoo-define ,exec ',name (lambda () ,@body)))

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
    (define-lisp-word + (exec)
      (lifoo-push exec (+ (lifoo-pop exec) (lifoo-pop exec))))
    
    (define-lisp-word * (exec)
      (lifoo-push exec (* (lifoo-pop exec) (lifoo-pop exec))))

    (define-lisp-word < (exec)
      (let ((rhs (lifoo-pop exec))
            (lhs (lifoo-pop exec)))
        (lifoo-push exec (< lhs rhs))))

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
      (let ((cnd (lifoo-parse exec (lifoo-pop exec)))
            (fn (lifoo-parse exec (lifoo-pop exec))))
        (when (eval `(progn ,@cnd))
          (eval `(progn ,@fn)))))
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
                 ((keywordp e)
                  (rec (rest ex) (cons `(lifoo-push ,exec ,e)
                                       acc)))
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall
                               ,(word-fn (gethash e (words exec))))
                             acc)))
                 (t
                  (rec (rest ex)
                       (cons `(lifoo-push ,exec ,e)
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
  (setf (gethash id (words exec)) (make-word :id id :fn fn)))

(defun lifoo-undefine (exec id)
  (remhash id (words exec)))

(defun lifoo-push (exec term)
  (push term (stack exec))
  term)

(defun lifoo-pop (exec)
  (pop (stack exec)))

(define-test (:lifoo :add)
  (assert (= 3 (do-lifoo ()
                 1 2 +))))

(define-test (:lifoo :list)
  (assert (= 2 (do-lifoo ()
                 (1 2 3) rest first))))

(define-test (:lifoo :list :map)
  (assert (equal '(2 4 6)
                 (do-lifoo ()
                   (1 2 3) (2 *) map))))

(define-test (:lifoo :print)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :stack)
  (assert (= 1 (do-lifoo ()
                 1 dup drop)))
  (assert (= 2 (do-lifoo ()
                 1 2 swap drop))))

(define-test (:lifoo :when)
  (assert (eq :ok (do-lifoo () :ok (1 2 <) when))))
