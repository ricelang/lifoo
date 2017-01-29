(defpackage lifoo
  (:use cl cl4l-index cl4l-utils))

(in-package lifoo)

(defparameter *lifoo* (make-lifoo))

(defmacro with-lifoo ((&key (env *lifoo*)) &body body)
  `(let ((*lifoo* ,env))
     ,@body))

(defstruct (env (:conc-name lifoo-))
  stack (defs (index #'lifoo-id)))

(defstruct (def (:conc-name lifoo-))
  id src imp)

(defun make-lifoo ()
  (let ((env (make-env)))
    (with-lifoo (:env env)
      (lifoo-def '+ :imp (lambda ()
                           (lifoo-push (+ (lifoo-pop) 
                                          (lifoo-pop))))))
    env))

(defun lifoo-push (term &key (env *lifoo*))
  (push term (lifoo-stack env))
  term)

(defun lifoo-read (in &key (env *lifoo*))
  (when-let (term (read in nil))
    (cons (let ((def? (when (symbolp term)
                        (index-find (lifoo-defs env) term))))
            (if def? (lifoo-imp def?) term))
          (lifoo-read in))))

(defun lifoo-pop (&key (env *lifoo*))
  (pop (lifoo-stack env)))

(defun lifoo-undef (id &key (env *lifoo*))
  (index-remove (lifoo-defs env) id))

(defun lifoo-def (id &key (env *lifoo*)
                          src
                          (imp (with-input-from-string (in src)
                                 (lifoo-read in :env env))))
  (lifoo-undef id :env env)
  (index-add (lifoo-defs env) (make-def :id id :src src :imp imp)))
