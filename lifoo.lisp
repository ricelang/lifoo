(defpackage lifoo
  (:export define-lifoo-init define-lisp-ops define-lisp-word
           define-word do-lifoo
           lifoo-asseq lifoo-call lifoo-define lifoo-dump-log
           lifoo-env? lifoo-error lifoo-eval
           lifoo-get
           lifoo-init lifoo-init-comparisons lifoo-init-env
           lifoo-init-flow lifoo-init-io lifoo-init-lists
           lifoo-init-meta lifoo-init-numbers
           lifoo-init-stack lifoo-init-strings lifoo-init-threads
           lifoo-log
           lifoo-parse lifoo-pop lifoo-print-log lifoo-push
           lifoo-read lifoo-rem lifoo-repl
           lifoo-set lifoo-stack
           lifoo-trace?
           lifoo-undefine
           lifoo-word make-lifoo
           with-lifoo with-lifoo-env
           *lifoo* *lifoo-env*)
  (:use bordeaux-threads cl cl4l-chan cl4l-clone cl4l-compare
        cl4l-test cl4l-utils))

(in-package lifoo)

(defvar *lifoo* nil)
(defvar *lifoo-env* nil)

(defmacro define-lisp-word (name
                            (&key (env? t) exec)
                            &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (make-lifoo-word :id ,(keyword! name)
                                    :env? ,env?
                                    :source ',body
                                    :compiled (lambda ()
                                                ,@body)))))

(defmacro define-lisp-ops ((&key exec) &rest ops)
  "Defines new words in EXEC for OPS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) (:env? nil)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 ops))))

(defmacro define-word (name (&key (env? t) exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (make-lifoo-word :id ,(keyword! name)
                                    :env? ,env?
                                    :source ',body))))

(defmacro do-lifoo ((&key exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo*
                          (lifoo-init :exec (make-lifoo))))
     (lifoo-eval ',body)
     (lifoo-pop)))

(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY pushes value equal to RES 
   according to COMPARE"
  `(asseq ,res (do-lifoo () ,@body)))

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
  (stack (make-array 3 :adjustable t :fill-pointer 0))
  logs
  (words (make-hash-table :test 'eq)))

(defstruct (lifoo-word (:conc-name))
  id
  env? trace?
  source parsed compiled)

(define-condition lifoo-error (simple-error)
  ((message :initarg :message :reader lifoo-error)))

(defun lifoo-parse (expr &key (exec *lifoo*))
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((rec (ex acc)
         (if ex
             (let ((e (first ex)))
               (cond
                 ((consp e)
                  (rec (rest ex)
                       (cons `(lifoo-push ',e) acc)))
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
                         (cons `(lifoo-call ,found?) acc))))
                 ((lifoo-word-p e)
                  (rec (rest ex)
                       (cons `(lifoo-call ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,e) acc)))))
             (nreverse acc))))
    (with-lifoo (:exec exec)
      (rec (list! expr) nil))))

(defparameter *eof* (gensym))

(defun lifoo-read (&key (in *standard-input*))
  (let ((more?) (expr))
    (do-while ((not (eq (setf more? (read in nil *eof*)) *eof*)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (eval `(progn ,@(lifoo-parse expr)))))

(defun lifoo-call (word &key (exec *lifoo*))
  (with-lifoo (:exec exec)
    (let ((fn (or (compiled word)
                  (setf (compiled word)
                        (eval
                         `(lambda ()
                            ,@(lifoo-parse (source word))))))))
      (when (trace? word)
        (push (list :enter (id word) (clone (stack exec)))
              (logs exec)))
      
      (if (env? word)
          (with-lifoo-env () (funcall fn))
          (funcall fn))
      
      (when (trace? word)
        (push (list :exit (id word) (clone (stack exec)))
              (logs exec))))))

(defun lifoo-define (id word &key (exec *lifoo*))
  "Defines ID as WORD in EXEC"  
  (setf (gethash (keyword! id) (words exec)) word))

(defun lifoo-undefine (id &key (exec *lifoo*))
  "Undefines word named ID in EXEC"  
  (remhash (keyword! id) (words exec)))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word named ID from EXEC or NIL if missing"  
  (gethash (keyword! id) (words exec)))

(defun lifoo-push (val &key (exec *lifoo*))
  "Pushes VAL onto EXEC stack"  
  (vector-push-extend val (stack exec))
  val)

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops and returns value from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (let ((val (vector-pop (stack exec))))
      val)))

(defun lifoo-peek (&key (exec *lifoo*))
  "Returns top of EXEC stack"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (unless (zerop fp)
        (aref stack (1- fp)))))

(defun (setf lifoo-peek) (val &key (exec *lifoo*))
  "Replaces top of EXEC stack with VAL"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (assert (not (zerop fp)))
    (setf (aref stack (1- fp)) val)))

(defun lifoo-env? (word)
  "Returns T if WORD runs in fresh environment, otherwise NIL"
  (trace? word))

(defun (setf lifoo-env?) (on? word)
  "Enables/disables env? for WORD"
  (setf (env? word) on?))

(defun lifoo-trace? (word)
  "Returns T if WORD is traced, otherwise NIL"
  (trace? word))

(defun (setf lifoo-trace?) (on? word)
  "Enables/disables trace for WORD"
  (setf (trace? word) on?))

(defun lifoo-log (msg &key (exec *lifoo*))
  "Logs MSG in EXEC"
  (push (list :log msg) (logs exec)))

(defun lifoo-dump-log (&key (exec *lifoo*))
  "Returns logs from EXEC"
  (let ((log(logs exec)))
    (setf (logs exec) nil)
    log))

(defun lifoo-print-log (log &key (out *standard-output*))
  "Prints log to OUT for EXEC"
  (dolist (e log)
    (apply #'format out
           (ecase (first e)
             (:enter "ENTER ~a ~a~%")
             (:exit  "EXIT  ~a ~a~%")
             (:log   "LOG   ~a~%"))
           (rest e))))

(defun lifoo-stack (&key (exec *lifoo*))
  "Returns current stack for EXEC"
  (stack exec))

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
               (lifoo-eval (lifoo-read :in in))
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
  (define-lisp-word :env (:env? nil)
    (lifoo-push (copy-list *lifoo-env*)))

  ;; Pops $var and returns value
  (define-lisp-word :get (:env? nil)
    (lifoo-push (lifoo-get (lifoo-pop))))

  ;; Pops $val and $var,
  ;; sets $var's value to $val and pushes $val
  (define-lisp-word :set (:env? nil)
    (let ((val (lifoo-pop))
          (var (lifoo-pop)))
      (lifoo-set var val)
      (lifoo-push val)))

  (define-lisp-word :rem (:env? nil)
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
  (define-word :unless () eval nil? when)

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
                   (setf res (lifoo-peek)))
                 res)
        (lifoo-pop)))))

(define-lifoo-init init-io
  ;; Pops $val and prints it
  (define-lisp-word :print ()
    (princ (lifoo-pop)))

  ;; Prints line ending
  (define-lisp-word :ln ()
    (terpri)))

(define-lifoo-init init-seqs
  (define-lisp-ops () cons)

  ;; Pushes stack as list and clears stack
  (define-lisp-word :list ()
    (let ((lst (map 'list #'identity (stack *lifoo*))))
      (setf (fill-pointer (stack *lifoo*)) 0)
      (lifoo-push lst))) 

  ;; Pops $list and pushes first element
  (define-lisp-word :first ()
    (lifoo-push (first (lifoo-pop))))

  ;; Pops $list and pushes rest
  (define-lisp-word :rest ()
    (lifoo-push (rest (lifoo-pop))))

  ;; Pops item from $1 and pushes it
  (define-lisp-word :pop ()
    (let* ((seq (lifoo-peek))
           (it (cond
                 ((arrayp seq)
                  (vector-pop seq))
                 (t
                  (pop (lifoo-peek))))))
      (lifoo-push it)))

  ;; Pops $it and pushes it on $1
  (define-lisp-word :push ()
    (let ((it (lifoo-pop))
          (seq (lifoo-peek)))
      (cond
        ((arrayp seq)
         (vector-push-extend it seq))
        (t
         (push it (lifoo-peek))))))

  ;; Pops $seq and pushes reversed
  (define-lisp-word :reverse ()
    (lifoo-push (reverse (lifoo-pop))))

  ;; Pops $fn and $seq,
  ;; and pushes result of mapping $fn over $seq
  (define-lisp-word :map ()
    (let ((expr (lifoo-pop)) (seq (lifoo-pop)))
      (lifoo-push (map
                   (cond
                     ((stringp seq) 'string)
                     ((arrayp seq) 'array)
                     (t 'list))
                   (eval `(lambda (it)
                            (lifoo-push it)
                            ,@(lifoo-parse expr)
                            (lifoo-pop)))
                   seq)))))

(define-lifoo-init init-meta
  ;; Pops $val and pushes its symbolic representation
  (define-lisp-word :symbol ()
    (lifoo-push (keyword! (lifoo-pop))))

  ;; Pops $val and pushes the word it represents
  (define-lisp-word :word ()
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push word)))

  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? ()
    (lifoo-push (null (lifoo-pop))))

  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp ()
    (let ((expr (lifoo-pop)))
      (lifoo-push (make-lifoo-word
                   :source expr
                   :compiled (eval `(lambda () ,expr))))))
  
  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval ()
    (lifoo-eval (lifoo-pop)))
  
  ;; Pops $word and enables tracing
  (define-lisp-word :trace ()
    (setf (trace? (lifoo-word (lifoo-pop))) t))

  ;; Pops $word and disabled tracing,
  ;; disables all if T
  (define-lisp-word :untrace ()
    (let ((expr (lifoo-pop)))
      (if (eq t expr)
          (do-hash-table ((words *lifoo*) _ w)
            (setf (trace? w) nil))
          (setf (trace? (lifoo-word expr)) nil))))

  ;; Pops $msg and logs it
  (define-lisp-word :log ()
    (lifoo-log (lifoo-pop)))

  ;; Dumps log on stack
  (define-lisp-word :dump-log ()
    (lifoo-push (lifoo-dump-log)))

  ;; Prints log
  (define-lisp-word :print-log ()
    (lifoo-print-log (lifoo-dump-log)))

  ;; Pops $msg and signals error
  (define-lisp-word :error ()
    (signal 'lifoo-error :message (lifoo-pop)))

  ;; Pops $cnd and signals error if NIL
  (define-lisp-word :assert ()
    (let* ((cnd (lifoo-pop))
           (ok? (progn (lifoo-eval cnd) (lifoo-pop))))
      (unless ok?
        (signal 'lifoo-error
                :message (format nil "assert failed: ~a" cnd))))))

(define-lifoo-init init-numbers
  (define-lisp-ops () + - * / = /= < > cons)

  (define-lisp-word :inc ()
    (incf (lifoo-peek)))

  (define-lisp-word :dec ()
    (decf (lifoo-peek))))

(define-lifoo-init init-stack
  ;; Pushes stack on stack as list
  (define-lisp-word :stack ()
    (lifoo-push (stack *lifoo*)))
  
  ;; Pops stack
  (define-lisp-word :drop ()
    (lifoo-pop))

  ;; Swaps $1 and $2
  (define-lisp-word :swap ()
    (let ((x (lifoo-pop)) (y (lifoo-pop)))
      (lifoo-push x)
      (lifoo-push y)))  
  
  ;; Pushes $1 on stack
  (define-lisp-word :dup ()
    (lifoo-push (lifoo-peek))))

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
      (chan-put (lifoo-peek) msg)))

  ;; Gets and pushes message from channel in $1
  (define-lisp-word :chan-get ()
    (let ((msg (chan-get (lifoo-peek))))
      (lifoo-push msg))))

(define-lifoo-init init
  (lifoo-init-meta)
  (lifoo-init-stack)
  (lifoo-init-flow)
  (lifoo-init-numbers)
  (lifoo-init-strings)
  (lifoo-init-seqs)
  (lifoo-init-comparisons)
  (lifoo-init-env)
  (lifoo-init-io)
  (lifoo-init-threads))
