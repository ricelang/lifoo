(defpackage lifoo
  (:use cl cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lifoo (name (exec) &body body)
  `(lifoo-define ,exec ',name (lambda () ,@body)))

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
  (let ((cx (make-exec)))
    (define-lifoo + (cx)
      (lifoo-push cx (+ (lifoo-pop cx) (lifoo-pop cx))))
    
    (define-lifoo * (cx)
      (lifoo-push cx (* (lifoo-pop cx) (lifoo-pop cx))))

    (define-lifoo drop (cx)
      (lifoo-pop cx))

    (define-lifoo dup (cx)
      (lifoo-push cx (first (stack cx))))

    (define-lifoo first (cx)
      (lifoo-push cx (first (lifoo-pop cx))))

    (define-lifoo ln (cx)
      (terpri))

    (define-lifoo map (cx)
      (let ((fn (lifoo-compile cx (lifoo-pop cx)))
            (lst (lifoo-pop cx)))
        (lifoo-push cx (mapcar (lambda (it)
                                 (lifoo-push cx it)
                                 (funcall fn)
                                 (lifoo-pop cx))
                               lst))))

    (define-lifoo print (cx)
      (princ (lifoo-pop cx)))

    (define-lifoo rest (cx)
      (lifoo-push cx (rest (lifoo-pop cx))))

    (define-lifoo swap (cx)
      (push (lifoo-pop cx) (rest (stack cx))))

    cx))

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
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall
                               ,(word-fn
                                 (gethash e (words exec))))
                             acc)))
                 (t
                  (rec (rest ex)
                       (cons `(lifoo-push ,exec ,e)
                             acc)))))
             (nreverse acc))))
    (rec expr nil)))

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
