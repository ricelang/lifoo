(defpackage lifoo
  (:use cl cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lifoo (name (context) &body body)
  `(lifoo-define ,context ',name (lambda () ,@body)))

(defmacro do-lifoo ((context) &body body)
  `(funcall (lifoo-compile (or ,context (make-lifoo)) '(,@body))))

(defstruct (lifoo-context (:conc-name)
                          (:constructor make-context))
  stack (words (make-hash-table :test 'eq)))

(defstruct (lifoo-word (:conc-name word-)
                       (:constructor make-word))
  id fn)

(defun make-lifoo ()
  (let ((cx (make-context)))
    (define-lifoo + (cx)
      (lifoo-push cx (+ (lifoo-pop cx) (lifoo-pop cx))))
    (define-lifoo drop (cx)
      (lifoo-pop cx))
    (define-lifoo dup (cx)
      (lifoo-push cx (first (stack cx))))
    (define-lifoo head (cx)
      (lifoo-push cx (first (lifoo-pop cx))))
    (define-lifoo ln (cx)
      (terpri))
    (define-lifoo print (cx)
      (princ (lifoo-pop cx)))
    (define-lifoo swap (cx)
      (push (lifoo-pop cx) (rest (stack cx))))
    (define-lifoo tail (cx)
      (lifoo-push cx (rest (lifoo-pop cx))))
    cx))

(defun lifoo-parse (context expr)
  (labels
      ((rec (ex acc &key (push? t))
         (if ex
             (let ((e (first ex)))
               (cond
                 ((consp e)
                  (rec (rest ex)
                       (cons `(lifoo-push
                               ,context
                               ,(rec e (list 'list) :push? nil))
                             acc)
                       :push? push?))
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall
                               ,(word-fn
                                 (gethash e (words context))))
                             acc)
                       :push? push?))
                 (t
                  (rec (rest ex)
                       (cons (if push?
                                 `(lifoo-push ,context ,e)
                                 e)
                             acc)
                       :push? push?))))
             (nreverse acc))))
    (rec expr nil)))

(defun lifoo-compile (context expr)
  (eval `(lambda ()
           ,@(lifoo-parse context expr)
           (lifoo-pop ,context))))

(defun lifoo-define (context id fn)
  (lifoo-undefine context id)
  (setf (gethash id (words context)) (make-word :id id :fn fn)))

(defun lifoo-undefine (context id)
  (remhash id (words context)))

(defun lifoo-push (context term)
  (push term (stack context))
  term)

(defun lifoo-pop (context)
  (pop (stack context)))

(define-test (:lifoo :add)
  (assert (= 3 (do-lifoo (nil)
                 1 2 +))))

(define-test (:lifoo :list)
  (assert (= 2 (do-lifoo (nil)
                 (1 2 3) tail head))))

(define-test (:lifoo :print)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo (nil)
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :stack)
  (assert (= 1 (do-lifoo (nil)
                 1 dup drop)))
  (assert (= 2 (do-lifoo (nil)
                 1 2 swap drop))))
