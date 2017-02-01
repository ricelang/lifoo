(defpackage lifoo
  (:export define-lifoo-init define-lisp-ops define-lisp-word
           define-word do-lifoo
           lifoo-assert lifoo-compile lifoo-define
           lifoo-error lifoo-eval
           lifoo-get
           lifoo-init lifoo-init-comparisons lifoo-init-env
           lifoo-init-flow lifoo-init-io lifoo-init-lists
           lifoo-init-meta lifoo-init-numbers
           lifoo-init-stack
           lifoo-init-strings lifoo-init-threads
           lifoo-parse lifoo-pop lifoo-push
           lifoo-read lifoo-rem lifoo-repl
           lifoo-set lifoo-stack
           lifoo-trace lifoo-top
           lifoo-undefine lifoo-untrace
           lifoo-word make-lifoo
           with-lifoo with-lifoo-env
           *lifoo* *lifoo-env*)
  (:use bordeaux-threads cl cl4l-chan cl4l-clone cl4l-compare
        cl4l-test cl4l-utils))

(in-package lifoo)

(defvar *lifoo* nil)
(defvar *lifoo-env* nil)

(defmacro define-lisp-word (name
                            (&key (copy-env? t) exec)
                            &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name (lambda ()
                            ,(if copy-env?
                                 `(with-lifoo-env ()
                                    ,@body)
                                 `(progn ,@body))))))

(defmacro define-lisp-ops ((&key exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op)
                        (:copy-env? nil)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (&key (copy-env? t) exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (lifoo-compile '(,@body)
                                  :copy-env? ,copy-env?))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo*
                          (lifoo-init :exec (make-lifoo))))
     (lifoo-eval '(,@body))
     (lifoo-pop)))

(defmacro with-lifoo ((&key exec) &body body)
  "Runs body with *LIFOO* bound to EXEC or new"
  `(let ((*lifoo* (or ,exec (lifoo-init :exec (make-lifoo)))))
     ,@body))

(defmacro with-lifoo-env ((&key env) &body body)
  "Runs body with *LIFOO-ENV* bound to ENV or copy"
  `(let ((*lifoo-env* (or ,env (copy-list *lifoo-env*))))
     ,@body))

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  stack traces tracing?
  (words (make-hash-table :test 'eq)))

(define-condition lifoo-error (error)
  ((message :initarg :message :reader lifoo-error)))

(define-condition lifoo-assert (lifoo-error) ())

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
                  (let ((found? (or (lifoo-word e)
                                    (error "missing word: ~a" e))))
                    (rec (rest ex)
                         (cons
                          `(progn
                             (when (tracing? ,exec)
                               (push (format nil "CALL ~a" ',e)
                                     (traces ,exec)))
                             (funcall ,found?))
                          acc))))
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

(defun lifoo-eval (expr &key (copy-env? t) (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (let ((pe (lifoo-parse expr)))
      (eval (if copy-env?
                `(with-lifoo-env ()
                   ,@pe)
                `(progn ,@pe))))))

(defun lifoo-compile (expr &key (copy-env? t) (exec *lifoo*))
  "Returns result of parsing and compiling EXPR in EXEC"  
  (eval (if copy-env?
            `(lambda ()
               (with-lifoo-env ()
                 ,@(lifoo-parse expr :exec exec)))
            `(lambda ()
               ,@(lifoo-parse expr :exec exec)))))

(defun lifoo-define (id fn &key (exec *lifoo*))
  "Defines word named ID in EXEC as FN"  
  (setf (gethash (keyword! id) (words exec)) fn))

(defun lifoo-undefine (id &key (exec *lifoo*))
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (words exec)))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word named ID from EXEC or NIL if missing"  
  (gethash (keyword! id) (words exec)))

(defun lifoo-push (expr &key (exec *lifoo*))
  "Pushes EXPR onto EXEC's stack"  
  (push expr (stack exec))
  (when (tracing? exec)
    (push (format nil "PUSH ~a~%~a" expr (stack exec))
          (traces exec)))
  expr)

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops EXPR from EXEC's stack"
  (when (stack exec)
    (let ((val (pop (stack exec))))
      (when (tracing? exec)
        (push (format nil "POP  ~a~%~a" val (stack exec))
              (traces exec)))
      val)))

(defun lifoo-top (&key (exec *lifoo*))
  "Returns top of EXEC's stack"
  (first (stack exec)))

(defun lifoo-stack (&key (exec *lifoo*))
  "Returns current stack for EXEC"
  (stack exec))

(defun lifoo-trace (&key (exec *lifoo*))
  "Enables tracing for EXEC"
  (setf (tracing? exec) t)
  (setf (traces exec) nil))

(defun lifoo-untrace (&key (exec *lifoo*))
  "Disables tracing for EXEC"
  (setf (tracing? exec) nil)
  (traces exec))

(defun lifoo-get (var)
  "Returns value of VAR in EXEC"
  (rest (assoc var *lifoo-env* :test #'eq))) 

(defun lifoo-set (var val)
  "Sets value of VAR in EXEC to VAL"
  (let ((found? (assoc var *lifoo-env* :test #'eq)))
    (if found?
        (rplacd found? val)
        (setf *lifoo-env* (acons var val *lifoo-env*))))
  val)

(defun lifoo-rem (var)
  "Returns value of VAR in EXEC"
  (setf *lifoo-env*
        (delete var *lifoo-env* :key #'first :test #'eq))) 

(defun lifoo-repl (&key (exec (lifoo-init :exec (make-lifoo)))
                        (in *standard-input*)
                        (prompt "Lifoo>")
                        (out *standard-output*))
  "Starts a REPL for EXEC"
  (with-lifoo (:exec exec)
    (with-lifoo-env ()
      (tagbody
       start
         (format out "~%~a " prompt)
         (when-let (line (read-line in nil))
           (unless (string= "" line)
             (with-input-from-string (in line)
               (lifoo-eval (lifoo-read :in in) :copy-env? nil)
               (format out "~a~%" (lifoo-pop)))
             (go start)))))))

(defmacro define-lifoo-init (name &body body)
  "Defines NAME-init around BODY with exec"
  `(defun ,(symbol! 'lifoo- name) (&key (exec *lifoo*)) 
     (with-lifoo (:exec exec) ,@body)
     exec))

(define-lifoo-init init-comparisons
  ;; Pops $rhs and $lhs,
  ;; and pushes result of comparing $lhs to $rhs
  (define-lisp-word :cmp ()
    (let ((rhs (lifoo-pop))
          (lhs (lifoo-pop)))
      (lifoo-push (compare lhs rhs))))
  
  (define-word :eq? () cmp 0 =)
  (define-word :neq? () cmp 0 /=)
  (define-word :lt? () cmp -1 =)
  (define-word :gt? () cmp 1 =)
  (define-word :lte? () cmp 1 <)
  (define-word :gte? () cmp -1 >))

(define-lifoo-init init-env
  ;; Pushes copy of env as alist
  (define-lisp-word :env (:copy-env? nil)
    (lifoo-push (copy-list *lifoo-env*)))

  ;; Pops $var and returns value
  (define-lisp-word :get (:copy-env? nil)
    (lifoo-push (lifoo-get (lifoo-pop))))

  ;; Pops $val and $var,
  ;; sets $var's value to $val and pushes $val
  (define-lisp-word :set (:copy-env? nil)
    (let ((val (lifoo-pop))
          (var (lifoo-pop)))
      (lifoo-set var val)
      (lifoo-push val)))

  (define-lisp-word :rem (:copy-env? nil)
    (let* ((var (lifoo-pop))
           (val (lifoo-get var)))
      (lifoo-rem var)
      (lifoo-push val))))

(define-lifoo-init init-flow
  ;; Pops $cnd and $res;
  ;; and pushes $res if $cnd, otherwise NIL
  (define-lisp-word :when ()
    (let ((cnd (lifoo-pop))
          (res (lifoo-pop)))
      (lifoo-eval cnd)
      (if (lifoo-pop)
          (lifoo-eval res)
          (lifoo-push nil))))

  ;; Pops $cnd and $res;
  ;; and pushes $res unless $cnd, otherwise NIL
  (define-word :unless () nil? when)

  ;; Pops $reps and $body;
  ;; and repeats $body $reps times,
  ;; pushing indexes before evaluating body
  (define-lisp-word :times ()
    (let ((reps (lifoo-pop))
          (body (lifoo-parse (lifoo-pop))))
      (dotimes (i reps)
        (lifoo-push i)
        (eval `(progn ,@body)))))

  ;; Pops $body and loops until $body pushes nil 
  (define-lisp-word :while ()
    (let ((body (lifoo-parse (lifoo-pop))) (res))
      (do-while ((progn
                   (eval `(progn ,@body))
                   (setf res (lifoo-top)))
                 res)
        (lifoo-pop)))))

(define-lifoo-init init-io
  ;; Pops $val and prints it
  (define-lisp-word :print ()
    (princ (lifoo-pop)))

  ;; Prints line ending
  (define-lisp-word :ln ()
    (terpri)))

(define-lifoo-init init-lists
  (define-lisp-ops () cons)

  ;; Clears stack and pushes previous contents as list
  (define-lisp-word :list ()
    (let ((lst (stack *lifoo*)))
      (setf (stack *lifoo*) nil)
      (lifoo-push (nreverse lst))))

  ;; Pops $list and pushes first element
  (define-lisp-word :first ()
    (lifoo-push (first (lifoo-pop))))

  ;; Pops $list and pushes rest
  (define-lisp-word :rest ()
    (lifoo-push (rest (lifoo-pop))))

  ;; Pops item from list in $1 and pushes it
  (define-lisp-word :pop ()
    (let ((it (pop (first (stack *lifoo*)))))
      (lifoo-push it)))

  ;; Pops $it and pushes it on list in $1
  (define-lisp-word :push ()
    (let ((it (lifoo-pop)))
      (push it (first (stack *lifoo*)))))

  ;; Pops $list and pushes reversed list
  (define-lisp-word :reverse ()
    (lifoo-push (reverse (lifoo-pop))))

  ;; Pops $fn and $lst,
  ;; and pushes result of mapping $fn over $lst
  (define-lisp-word :map ()
    (let ((fn (lifoo-compile (lifoo-pop)))
          (lst (lifoo-pop)))
      (lifoo-push (mapcar (lambda (it)
                            (lifoo-push it)
                            (funcall fn)
                            (lifoo-pop))
                          lst)))))

(define-lifoo-init init-meta
  ;; Pops $val and pushes its symbolic representation
  (define-lisp-word :symbol ()
    (lifoo-push (keyword! (lifoo-pop))))

  ;; Pops $val and pushes the word it represents
  (define-lisp-word :word ()
    (let ((fn (lifoo-word (lifoo-pop))))
      (lifoo-push fn)))

  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? ()
    (lifoo-push (null (lifoo-eval (lifoo-pop)))))

  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp ()
    (lifoo-push (eval `(lambda () ,(lifoo-pop)))))
  
  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval ()
    (lifoo-push (lifoo-eval (lifoo-pop))))
  
  ;; Pops $expr and pushes compiled word
  (define-lisp-word :compile ()
    (lifoo-push (lifoo-compile (lifoo-pop))))

  ;; Enables tracing and clears trace
  (define-lisp-word :trace ()
    (setf (tracing? *lifoo*) t)
    (setf (traces *lifoo*) nil))

  ;; Disables tracing and prints trace
  (define-lisp-word :untrace ()
    (dolist (st (reverse (traces *lifoo*)))
      (format t "~a~%" st))
    (setf (tracing? *lifoo*) nil))

  ;; Pops $msg and signals error
  (define-lisp-word :error ()
    (signal 'lifoo-error :message (lifoo-eval (lifoo-pop))))

  ;; Pops $cnd and signals error if NIL
  (define-lisp-word :assert ()
    (let* ((cnd (lifoo-pop))
           (ok? (progn (lifoo-eval cnd) (lifoo-pop))))
      (unless ok?
        (signal 'lifoo-assert
                :message (format nil "assert failed: ~a" cnd))))))

(define-lifoo-init init-numbers
  (define-lisp-ops () + - * / = /= < > cons)

  (define-lisp-word :inc ()
    (incf (first (stack *lifoo*)))))

(define-lifoo-init init-stack
  ;; Pushes stack on stack as list
  (define-lisp-word :stack ()
    (lifoo-push (copy-list (stack *lifoo*))))
  
  ;; Pops stack
  (define-lisp-word :drop ()
    (lifoo-pop))

  ;; Swaps $1 and $2
  (define-lisp-word :swap ()
    (push (lifoo-pop) (rest (stack *lifoo*))))
  
  ;; Pushes $1 on stack
  (define-lisp-word :dup ()
    (lifoo-push (first (stack *lifoo*)))))

(define-lifoo-init init-strings
  ;; Pops $val and pushes string representation
  (define-lisp-word :string ()
    (let ((val (lifoo-pop)))
      (lifoo-push (if (listp val)
                      (apply #'string! val)
                      (string! val)))))

  ;; Pops $args and $fmt,
  ;; and pushes formatted output
  (define-lisp-word :format ()
    (let ((args (lifoo-pop))
          (fmt (lifoo-pop)))
      (lifoo-push (apply #'format nil fmt args)))))

(define-lifoo-init init-threads
  ;; Yields processor and re-schedules thread
  (define-lisp-word :yield ()
    (thread-yield))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :sleep ()
    (sleep (lifoo-pop)))

  ;; Pops $expr;
  ;; evaluates it in new thread/exec,
  ;; and pushes thread
  (define-lisp-word :thread ()
    (let* ((expr (lifoo-pop))
           (exec (make-lifoo :stack (clone (stack *lifoo*))
                             :words (clone (words *lifoo*))))
           (thread (make-thread (lambda ()
                                  (lifoo-eval expr
                                              :exec exec)))))
      (lifoo-push thread)))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :join-thread ()
    (lifoo-push (join-thread (lifoo-pop))))

  ;; Pops $buf-len and pushes new channel
  (define-lisp-word :chan ()
    (lifoo-push (make-chan :max-length (lifoo-pop))))

  ;; Pops $msg and puts on channel in $1
  (define-lisp-word :chan-put ()
    (let ((msg (lifoo-pop)))
      (chan-put (first (stack *lifoo*)) msg)))

  ;; Gets and pushes message from channel in $1
  (define-lisp-word :chan-get ()
    (let ((msg (chan-get (first (stack *lifoo*)))))
      (lifoo-push msg))))

(define-lifoo-init init
  (lifoo-init-meta)
  (lifoo-init-stack)
  (lifoo-init-flow)
  (lifoo-init-numbers)
  (lifoo-init-strings)
  (lifoo-init-lists)
  (lifoo-init-comparisons)
  (lifoo-init-env)
  (lifoo-init-io)
  (lifoo-init-threads))
