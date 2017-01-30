(defpackage lifoo
  (:use cl cl4l-index cl4l-test cl4l-utils))

(in-package lifoo)

(defmacro define-lifoo (name (context) &body body)
  `(lifoo-define ,context ',name (lambda () ,@body)))

(defmacro do-lifoo ((context) &body body)
  `(funcall (lifoo-compile (or ,context (make-lifoo)) '(,@body))))

(defstruct (lifoo-context (:conc-name)
                          (:constructor make-context))
  stack (words (index #'word-id)))

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
    (define-lifoo ln (cx)
      (terpri))
    (define-lifoo print (cx)
      (princ (lifoo-pop cx)))
    (define-lifoo swap (cx)
      (push (lifoo-pop cx) (rest (stack cx))))
    cx))

(defun lifoo-compile (context expr)
  (labels ((rec (es acc)
             (if es
                 (let ((e (first es)))
                   (cond
                     ((consp e)
                      (rec (rest es) (cons (rec e nil) acc)))
                     ((symbolp e)
                      (rec (rest es)
                           (cons `(funcall
                                   ,(word-fn
                                     (index-find (words context)
                                                 e)))
                                 acc)))
                     (t (rec (rest es)
                             (cons `(lifoo-push ,context ,e)
                                   acc)))))
                 (nreverse acc))))
    (eval `(lambda ()
             ,@(rec expr nil)
             (lifoo-pop ,context)))))

(defun lifoo-define (context id fn)
  (lifoo-undefine context id)
  (index-add (words context) (make-word :id id :fn fn)))

(defun lifoo-undefine (context id)
  (index-remove (words context) id))

(defun lifoo-push (context term)
  (push term (stack context))
  term)

(defun lifoo-pop (context)
  (pop (stack context)))

(define-test (:lifoo :add)
  (assert (= 3 (do-lifoo (nil) 1 2 +))))

(define-test (:lifoo :stack)
  (assert (= 1 (do-lifoo (nil) 1 dup drop)))
  (assert (= 2 (do-lifoo (nil) 1 2 swap drop))))

(define-test (:lifoo :print)
  (assert (string= (format nil "hello lifoo!~%")
           (with-output-to-string (out)
             (let ((*standard-output* out))
               (do-lifoo (nil) "hello lifoo!" print ln))))))
