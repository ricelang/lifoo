(defpackage lifoo
  (:export define-lisp-ops define-lisp-word define-word do-lifoo
           lifoo-compile lifoo-define
           lifoo-eval lifoo-init lifoo-parse lifoo-pop lifoo-push
           lifoo-read lifoo-repl lifoo-stack lifoo-stack-trace
           lifoo-stack-untrace lifoo-undefine lifoo-word
           lifoo-words make-lifoo
           *lifoo*)
  (:use cl cl4l-compare cl4l-test cl4l-utils))

(in-package lifoo)

(defvar *lifoo* nil)

(defmacro define-lisp-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name (lambda () ,@body))))

(defmacro define-lisp-ops ((&key exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) ()
                      (let ((,_rhs (lifoo-pop))
                            (,_lhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (lifoo-compile '(,@body)))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo*
                          (lifoo-init :exec (make-lifoo))))
     (lifoo-eval '(,@body))
     (lifoo-pop)))

(defmacro with-lifoo ((&key exec) &body body)
  `(let ((*lifoo* (or ,exec (lifoo-init :exec (make-lifoo)))))
     ,@body))

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  stack stack-trace stack-trace?
  (words (make-hash-table :test 'eq)))

(defun lifoo-parse (expr &key (exec *lifoo*))
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((rec (ex acc)
         (if ex
             (let ((e (first ex)))
               (cond
                 ((consp e)
                  (rec (rest ex)
                       (cons `(lifoo-push '(,@e)) acc)))
                 ((null e)
                  (rec (rest ex)
                       (cons `(lifoo-push nil) acc)))
                 ((eq e t)
                  (rec (rest ex) (cons `(lifoo-push t) acc)))
                 ((keywordp e)
                  (rec (rest ex) (cons `(lifoo-push ,e) acc)))
                 ((symbolp e)
                  (rec (rest ex)
                       (cons `(funcall ,(lifoo-word e)) acc)))
                 ((functionp e)
                  (rec (rest ex)
                       (cons `(funcall ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,e) acc)))))
             (nreverse acc))))
    (with-lifoo (:exec exec)
      (rec (list! expr) nil))))

(defun lifoo-read (&key (in *standard-input*))
  (let ((more?) (expr))
    (do-while ((setf more? (read in nil)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (let ((pe (lifoo-parse expr :exec exec)))
    (eval `(progn ,@pe))))

(defun lifoo-compile (expr &key (exec *lifoo*))
  "Returns result of parsing and compiling EXPR in EXEC"  
  (eval `(lambda ()
           ,@(lifoo-parse expr :exec exec))))

(defun lifoo-define (id fn &key (exec *lifoo*))
  "Defines word named ID in EXEC as FN"  
  (setf (gethash (keyword! id) (words exec)) fn))

(defun lifoo-undefine (id &key (exec *lifoo*))
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (words exec)))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word named ID from EXEC or signals error if missing"  
  (let ((word (gethash (keyword! id) (words exec))))
    (unless word (error "missing word: ~a" id))
    (when (stack-trace? exec)
      (push (format nil "WORD ~a" id)
            (stack-trace exec)))
    word))

(defun lifoo-push (expr &key (exec *lifoo*))
  "Pushes EXPR onto EXEC's stack"  
  (push expr (stack exec))
  (when (stack-trace? exec)
    (push (format nil "PUSH ~a~%~a" expr (stack exec))
          (stack-trace exec)))
  expr)

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops EXPR from EXEC's stack"
  (when (stack exec)
    (let ((val (pop (stack exec))))
      (when (stack-trace? exec)
        (push (format nil "POP  ~a~%~a" val (stack exec))
              (stack-trace exec)))
      val)))

(defun  lifoo-stack (&key (exec *lifoo*))
  (stack exec))

(defun lifoo-stack-trace (&key (exec *lifoo*))
  (setf (stack-trace? exec) nil)
  (stack-trace exec))

(defun lifoo-stack-untrace (&key (exec *lifoo*))
  (let ((trace (stack-trace exec)))
    (setf (stack-trace? exec) nil)
    trace))

(defun lifoo-words (&key (exec *lifoo*))
  (words exec))

(defun lifoo-repl (&key (exec (lifoo-init :exec (make-lifoo)))
                        (in *standard-input*)
                        (prompt "lifoo>")
                        (out *standard-output*))
  (with-lifoo (:exec exec) 
    (tagbody
     start
       (format out "~%~a " prompt)
       (when-let (line (read-line in nil))
         (unless (string= "" line)
           (with-input-from-string (in line)
             (lifoo-eval (lifoo-read :in in))
             (format out "~a~%" (lifoo-pop)))
           (go start))))))

(defun lifoo-init (&key (exec *lifoo*))
  "Initializes EXEC with built-in words"
  (with-lifoo (:exec exec)
    ;; Define binary ops
    (define-lisp-ops () + - * / = /= < > cons)

    ;; Replaces $1 and $2 with result of comparing $2 to $1
    (define-lisp-word :cmp ()
      (let ((rhs (lifoo-pop))
            (lhs (lifoo-pop)))
        (lifoo-push (compare lhs rhs))))

    ;; Derived comparison ops
    (define-word :eq? () cmp 0 =)
    (define-word :neq? () cmp 0 /=)
    (define-word :lt? () cmp -1 =)
    (define-word :gt? () cmp 1 =)
    (define-word :lte? () cmp 1 <)
    (define-word :gte? () cmp -1 >)

    ;; Drops $1 from stack
    (define-lisp-word :drop ()
      (lifoo-pop))

    ;; Pushes $1 on stack
    (define-lisp-word :dup ()
      (lifoo-push (first (stack *lifoo*))))
    
    ;; Replaces $1 with result of evaluating it
    (define-lisp-word :eval ()
      (lifoo-push (lifoo-eval (lifoo-pop))))

    ;; Replaces $1 with result of compiling it
    (define-lisp-word :compile ()
      (lifoo-push (lifoo-compile (lifoo-pop))))

    ;; Replaces $1 with first element of list
    (define-lisp-word :first ()
      (lifoo-push (first (lifoo-pop))))

    ;; Replaces $1 (arguments) and $2 (format) with formatted
    ;; output
    (define-lisp-word :format ()
      (let ((args (lifoo-pop))
            (fmt (lifoo-pop)))
        (lifoo-push (apply #'format nil fmt args))))

    ;; Clears and pushes stack
    (define-lisp-word :list ()
      (let ((lst (stack *lifoo*)))
        (setf (stack *lifoo*) nil)
        (lifoo-push (nreverse lst))))

    ;; Prints line ending
    (define-lisp-word :ln ()
      (terpri))

    ;; Replaces $1 and $2 with results of mapping $1 over $2
    (define-lisp-word :map ()
      (let ((fn (lifoo-compile (lifoo-pop)))
            (lst (lifoo-pop)))
        (lifoo-push (mapcar (lambda (it)
                              (lifoo-push it)
                              (funcall fn)
                              (lifoo-pop))
                            lst))))

    ;; Replaces $1 with T if NIL, otherwise NIL
    (define-lisp-word :nil? ()
      (lifoo-push (null (lifoo-eval (lifoo-pop)))))

    ;; Pops and prints $1
    (define-lisp-word :print ()
      (princ (lifoo-pop)))

    ;; Pops item from list in $1 and pushes it on stack
    (define-lisp-word :pop ()
      (let ((it (pop (first (stack *lifoo*)))))
        (lifoo-push it)))

    ;; Pops $1 from stack and pushes it on list in $2
    (define-lisp-word :push ()
      (let ((it (lifoo-pop)))
        (push it (first (stack *lifoo*)))))

    ;; Replaces $1 with rest of list
    (define-lisp-word :rest ()
      (lifoo-push (rest (lifoo-pop))))

    ;; Replaces $1 with reversed list
    (define-lisp-word :reverse ()
      (lifoo-push (reverse (lifoo-pop))))

    ;; Sleeps for $1 seconds
    (define-lisp-word :sleep ()
      (sleep (lifoo-pop)))

    ;; Enables stack tracing and clears trace
    (define-lisp-word :stack-trace ()
      (setf (stack-trace? *lifoo*) t)
      (setf (stack-trace *lifoo*) nil))

    ;; Disables stack tracing and prints trace
    (define-lisp-word :stack-untrace ()
      (dolist (st (reverse (stack-trace *lifoo*)))
        (format t "~a~%" st))
      (setf (stack-trace? *lifoo*) nil))

    ;; Replaces $1 with string representation
    (define-lisp-word :string ()
      (let ((val (lifoo-pop)))
        (lifoo-push (if (listp val)
                        (apply #'string! val)
                        (string! val)))))

    ;; Pops and repeats body in $2 x $1, pushing indexes on stack
    (define-lisp-word :times ()
      (let ((reps (lifoo-pop))
            (body (lifoo-parse (lifoo-pop))))
        (dotimes (i reps)
          (lifoo-push i)
          (eval `(progn ,@body)))))

    ;; Replaces $1 with results of converting it to a symbol
    (define-lisp-word :symbol ()
      (lifoo-push (keyword! (lifoo-pop))))
    
    ;; Swaps $1 and $2
    (define-lisp-word :swap ()
      (push (lifoo-pop) (rest (stack *lifoo*))))

    ;; Replaces $1 and $2 with results of evaluating $2 if $1,
    ;; otherwise NIL
    (define-lisp-word :when ()
      (let ((cnd (lifoo-pop))
            (res (lifoo-pop)))
        (lifoo-eval cnd)
        (if (lifoo-pop)
            (lifoo-eval res)
            (lifoo-push nil))))

    ;; Replaces $1 and $2 with results of evaluating $2 if $1
    ;; is NIL, otherwise NIL
    (define-word :unless () nil? when)

    ;; Replaces $1 with the word it represents
    (define-lisp-word :word ()
      (let ((fn (lifoo-word (lifoo-pop))))
        (lifoo-push fn))))
  exec)
