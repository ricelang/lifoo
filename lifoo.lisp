(defpackage lifoo
  (:export define-lifoo-init define-binary-words define-lisp-word
           define-word do-lifoo
           lifoo-call lifoo-define
           lifoo-del lifoo-dump-log
           lifoo-env lifoo-env? lifoo-error lifoo-eval
           lifoo-fn lifoo-get
           lifoo-init lifoo-init-arrays lifoo-init-basics
           lifoo-init-comparisons lifoo-init-env
           lifoo-init-flow lifoo-init-io lifoo-init-lists
           lifoo-init-meta lifoo-init-numbers lifoo-init-seqs
           lifoo-init-stack lifoo-init-strings lifoo-init-threads
           lifoo-log
           lifoo-parse lifoo-parse-word lifoo-pop lifoo-print-log
           lifoo-push
           lifoo-read lifoo-repl
           lifoo-stack
           lifoo-trace?
           lifoo-undefine
           lifoo-word make-lifoo with-lifoo
           *lifoo*)
  (:use bordeaux-threads cl cl4l-chan cl4l-clone cl4l-compare
        cl4l-test cl4l-utils))

(in-package lifoo)

(defvar *lifoo* nil)

(defmacro define-lisp-word (name (&key env? exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (make-lifoo-word :id ,(keyword! name)
                                    :env? ,env?
                                    :source ',body
                                    :fn (lambda () ,@body)))))

(defmacro define-binary-words ((&key env? exec) &rest forms)
  "Defines new words in EXEC for FORMS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op)
                        (:env? ,env? :exec ,exec)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 forms))))

(defmacro define-word (name (&key env? exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (make-lifoo-word :id ,(keyword! name)
                                    :env? ,env?
                                    :source ',body))))

(defmacro do-lifoo ((&key (env t) exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo*
                          (lifoo-init :exec (make-lifoo)))
                :env ,env)
     (lifoo-eval ',body)
     (lifoo-pop)))

(defmacro with-lifoo ((&key env exec) &body body)
  "Runs body with *LIFOO* bound to EXEC or new; environment
   is bound to ENV if not NIL, or copy of current if T"
  `(let ((*lifoo* (or ,exec (lifoo-init :exec (make-lifoo)))))
     (when ,env
       (push (if (eq t ,env) (copy-list (lifoo-env)) ,env)
             (envs *lifoo*)))
     (unwind-protect (progn ,@body)
       (when ,env (pop (envs *lifoo*))))))

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  (stack (make-array 3 :adjustable t :fill-pointer 0))
  envs logs
  (words (make-hash-table :test 'eq)))

(defstruct (lifoo-word (:conc-name))
  id
  env? trace?
  source fn)

(define-condition lifoo-error (simple-error) ()) 

(defun lifoo-error (fmt &rest args)
  (error 'lifoo-error :format-control fmt :format-arguments args))

(defun lifoo-read (&key (in *standard-input*))
  "Reads Lifoo code from IN until end of file"
  (let ((eof? (gensym)) (more?) (expr))
    (do-while ((not (eq (setf more? (read in nil eof?)) eof?)))
      (push more? expr))
    (nreverse expr)))

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
                  (let ((word (or (lifoo-word e)
                                  (error "missing word: ~a" e))))
                    (rec (rest ex)
                         (cons `(lifoo-call ,word) acc))))
                 ((lifoo-word-p e)
                  (rec (rest ex)
                       (cons `(lifoo-call ,e) acc)))
                 (t
                  (rec (rest ex) (cons `(lifoo-push ,e) acc)))))
             (nreverse acc))))
    (with-lifoo (:exec exec)
      (rec (list! expr) nil))))

(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (eval `(progn ,@(lifoo-parse expr)))))

(defun lifoo-fn (word &key (exec *lifoo*))
  "Returns compiled function for WORD"
  (or (fn word)
      (setf (fn word)
            (eval `(lambda ()
                     ,@(lifoo-parse (source word) :exec exec))))))

(defun lifoo-call (word &key (exec *lifoo*))
  "Calls WORD in EXEC"
  (when (trace? word)
    (push (list :enter (id word) (clone (stack exec)))
          (logs exec)))

  (with-lifoo (:exec exec :env (env? word))
    (funcall (lifoo-fn word)))

  (when (trace? word)
    (push (list :exit (id word) (clone (stack exec)))
          (logs exec))))

(defun lifoo-define (id word &key (exec *lifoo*))
  "Defines ID as WORD in EXEC"  
  (setf (gethash (keyword! id) (words exec)) word))

(defun lifoo-undefine (word &key (exec *lifoo*))
  "Undefines word for ID in EXEC"  
  (remhash (if (lifoo-word-p word)
               (id word)
               (keyword! word))
           (words exec)))

(defun lifoo-word (word &key (exec *lifoo*))
  "Returns WORD from EXEC, or NIL if missing"
  (if (lifoo-word-p word)
      word
      (gethash (keyword! word) (words exec))))

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
  "Returns T if WORD requires separate environment, otherwise NIL"
  (env? word))

(defun (setf lifoo-env?) (on? word)
  "Enables/disables separate environment for WORD"
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

(defun lifoo-env (&key (exec *lifoo*))
  "Returns current environment"
  (first (envs exec)))

(defun (setf lifoo-env) (env &key (exec *lifoo*))
  "Replaces current environment"
  (rplaca (envs exec) env))

(defun lifoo-get (var)
  "Returns value of VAR in EXEC"
  (rest (assoc var (lifoo-env) :test #'eq))) 

(defun (setf lifoo-get) (val var)
  "Sets value of VAR in EXEC to VAL"
  (push (cons var val) (lifoo-env))
  val)

(defun lifoo-del (var)
  "Deletes VAR from EXEC and returns value"
  (setf (lifoo-env)
        (delete var (lifoo-env) :key #'first :test #'eq))) 

(defun lifoo-repl (&key (exec (lifoo-init :exec (make-lifoo)))
                        (in *standard-input*)
                        (prompt "Lifoo>")
                        (out *standard-output*))
  "Starts a REPL for EXEC with input from IN and output to OUT,
   using PROMPT"
  (with-lifoo (:exec exec :env t)
    (tagbody
       start
         (format out "~%~a " prompt)
         (when-let (line (read-line in nil))
           (unless (string= "" line)
             (with-input-from-string (in line)
               (restart-case
                   (progn
                     (lifoo-eval (lifoo-read :in in))
                     (format out "~a~%" (lifoo-pop)))
                 (ignore ()
                   :report "Ignore error and continue.")))
             (go start))))))

(defmacro define-lifoo-init (name &body body)
  "Defines NAME-init around BODY with exec"
  `(defun ,(symbol! 'lifoo- name) (&key (exec *lifoo*)) 
     (with-lifoo (:exec exec) ,@body)
     exec))

(define-lifoo-init init-basics
  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? ()
    (lifoo-push (null (lifoo-pop))))

  ;; Pushes clone of $1
  (define-lisp-word :clone ()
    (lifoo-push (clone (lifoo-peek)))))

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
  ;; Pushes current environment on stack
  (define-lisp-word :env ()
    (lifoo-push (lifoo-env)))

  ;; Pops $var and returns value
  (define-lisp-word :get ()
    (lifoo-push (lifoo-get (lifoo-pop))))

  ;; Pops $val and $var,
  ;; sets $var's value to $val and pushes $val
  (define-lisp-word :set ()
    (let ((val (lifoo-pop))
          (var (lifoo-pop)))
      (setf (lifoo-get var) val)
      (lifoo-push val)))

  (define-lisp-word :del ()
    (let* ((var (lifoo-pop))
           (val (lifoo-get var)))
      (lifoo-del var)
      (lifoo-push val))))

(define-lifoo-init init-flow
  ;; Pops $cnd, $true and $false;
  ;; and pushes $true if $cnd, otherwise $false
  (define-lisp-word :cond (:env? t)
    (let ((cnd (lifoo-pop))
          (true (lifoo-pop))
          (false (lifoo-pop)))
      (lifoo-eval cnd)
      (lifoo-push (if (lifoo-pop) true false))))
  
  ;; Pops $cnd and $res;
  ;; and pushes $res if $cnd, otherwise NIL
  (define-lisp-word :when (:env? t)
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
  (define-lisp-word :times (:env? t)
    (let ((reps (lifoo-pop))
          (body (lifoo-parse (lifoo-pop))))
      (dotimes (i reps)
        (lifoo-push i)
        (eval (cons 'progn body)))))

  ;; Pops $body and loops until $body pushes nil 
  (define-lisp-word :while (:env? t)
    (let ((body (lifoo-parse (lifoo-pop))) (res))
      (do-while ((progn
                   (eval (cons 'progn body))
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
  ;; Pops $idx and pushes item from seq
  (define-lisp-word :nth ()
    (let* ((idx (lifoo-pop))
           (seq (lifoo-peek))
           (it (cond
                 ((arrayp seq)
                  (aref seq idx))
                 (t
                  (nth idx seq)))))
      (lifoo-push it)))

  ;; Pops $idx and $it,
  ;; and updates seq in $1
  (define-lisp-word :set-nth ()
    (let ((it (lifoo-pop))
          (idx (lifoo-pop))
          (seq (lifoo-peek)))
      (cond
        ((arrayp seq)
         (setf (aref seq idx) it))
        (t
         (setf (nth idx seq) it)))))

  ;; Pushes length of $1
  (define-lisp-word :length ()
    (let ((val (lifoo-peek)))
      (lifoo-push
       (cond
         ((chan? val)
          (chan-length val))
         (t
          (length val))))))
  
  ;; Pops item from seq in $1 and pushes it
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
  (define-lisp-word :map (:env? t)
    (let ((expr (lifoo-pop)) (seq (lifoo-pop)))
      (lifoo-push (map
                   (cond
                     ((stringp seq) 'string)
                     ((vectorp seq) 'vector)
                     (t 'list))
                   (eval `(lambda (it)
                            (lifoo-push it)
                            ,@(lifoo-parse expr)
                            (lifoo-pop)))
                   seq))))

  ;; Pops $pred and filters $1 by it
  (define-lisp-word :filter (:env? t)
    (let ((pred (lifoo-parse (lifoo-pop))))
      (setf (lifoo-peek)
            (remove-if (eval `(lambda (it)
                                (lifoo-push it)
                                ,@pred
                                (lifoo-pop)))
                       (lifoo-peek)))))

  ;; Pops $fn and replaces $1 with reduction by $fn
  (define-lisp-word :reduce (:env? t)
    (let ((fn (lifoo-parse (lifoo-pop))))
      (setf (lifoo-peek)
            (reduce (eval `(lambda (x y)
                             (lifoo-push x)
                             (lifoo-push y)
                             ,@fn
                             (lifoo-pop)))
                    (lifoo-peek))))))

(define-lifoo-init init-lists
  (define-binary-words () cons)

  ;; Pushes stack as list and clears stack
  (define-lisp-word :list ()
    (let ((lst (map 'list #'identity (stack *lifoo*))))
      (setf (fill-pointer (stack *lifoo*)) 0)
      (lifoo-push lst))) 

  ;; Pops $list and pushes rest
  (define-lisp-word :rest ()
    (lifoo-push (rest (lifoo-pop))))

  ;; Pops $list and pushes first element
  (define-lisp-word :first ()
    (lifoo-push (first (lifoo-pop)))))

(define-lifoo-init init-arrays
  ;; Pops $items and pushes new array
  (define-lisp-word :array ()
    (let* ((items (lifoo-pop))
           (len (length items)))
      (lifoo-push (make-array len
                              :initial-contents items
                              :adjustable t
                              :fill-pointer len)))))

(define-lifoo-init init-meta
  ;; Pops $val and pushes its symbolic representation
  (define-lisp-word :symbol ()
    (lifoo-push (keyword! (lifoo-pop))))

  ;; Pops $val and pushes the word it represents
  (define-lisp-word :word ()
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push word)))

  ;; Pops word and pushes source
  (define-lisp-word :source ()
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push (source word))))
  
  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp ()
    (let ((expr (lifoo-pop)))
      (lifoo-push (make-lifoo-word
                   :source expr
                   :fn (eval `(lambda () ,expr))))))
  
  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval (:env? t)
    (lifoo-eval (lifoo-pop)))

  ;; Pops $id and $body,
  ;; and defines and pushes word
  (define-lisp-word :define ()
    (let* ((id (keyword! (lifoo-pop)))
           (body (lifoo-pop))
           (word (make-lifoo-word :id id :source body)))
      (lifoo-define id word)
      (lifoo-push word)))

  ;; Pops $id;
  ;; and undefines and pushes T,
  ;; or NIL if not found
  (define-lisp-word :undefine ()
    (let ((id (keyword! (lifoo-pop))))
      (lifoo-push (lifoo-undefine id))))

  ;; Pops $word and enables tracing
  (define-lisp-word :trace ()
    (setf (trace? (lifoo-word (lifoo-pop))) t))

  ;; Pops $word and disabled tracing,
  ;; T disables all
  (define-lisp-word :untrace ()
    (let ((expr (lifoo-pop)))
      (if (eq t expr)
          (do-hash-table (_ w (words *lifoo*))
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
    (lifoo-error (lifoo-pop)))

  ;; Pops $cnd and signals error if NIL
  (define-lisp-word :assert ()
    (let* ((cnd (lifoo-pop))
           (ok? (progn (lifoo-eval cnd) (lifoo-pop))))
      (unless ok?
        (lifoo-error "assert failed: ~a" cnd))))

  ;; Pops $expected and $actual,
  ;; and signals error they don't compare equal  
  (define-lisp-word :asseq ()
    (let ((expected (lifoo-pop))
          (actual (lifoo-pop)))
      (unless (zerop (compare expected actual))
        (lifoo-error "assert failed: ~a /= ~a" actual expected)))))

(define-lifoo-init init-numbers
  (define-binary-words () + - * / = /= < > cons)

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
    (lifoo-push (lifoo-peek)))

  ;; Clears stack
  (define-lisp-word :reset ()
    (setf (fill-pointer (stack *lifoo*)) 0))

  (let ((var (gensym)))
    ;; Pushes backup of stack to environment
    (define-lisp-word :backup ()
      (push (copy-seq (stack *lifoo*)) (lifoo-get var)))

    ;; Pops and restores backup from environment
    (define-lisp-word :restore ()
      (let* ((prev (pop (lifoo-get var)))
             (curr (stack *lifoo*))
             (len (length prev)))
        (setf (fill-pointer curr) len)
        (dotimes (i len)
          (setf (aref curr i) (aref prev i)))))))

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
  (lifoo-init-basics)
  (lifoo-init-meta)
  (lifoo-init-stack)
  (lifoo-init-flow)
  (lifoo-init-numbers)
  (lifoo-init-strings)
  (lifoo-init-seqs)
  (lifoo-init-lists)
  (lifoo-init-arrays)
  (lifoo-init-comparisons)
  (lifoo-init-env)
  (lifoo-init-io)
  (lifoo-init-threads))
