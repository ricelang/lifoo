(defpackage lifoo
  (:export define-init define-binary-words define-lisp-word
           define-word do-lifoo
           lifoo-compile
           lifoo-break
           lifoo-call
           lifoo-define lifoo-define-macro lifoo-del lifoo-dump-log
           lifoo-env lifoo-error lifoo-eval
           lifoo-get
           lifoo-init lifoo-log lifoo-macro-word
           lifoo-parse lifoo-parse-word lifoo-peek lifoo-peek-set
           lifoo-pop lifoo-print-log lifoo-push
           lifoo-read lifoo-repl lifoo-reset
           lifoo-stack
           lifoo-trace?
           lifoo-undefine
           lifoo-word make-lifoo
           with-lifoo
           *lifoo* *lifoo-init*)
  (:use bordeaux-threads cl cl4l-chan cl4l-clone cl4l-compare
        cl4l-test cl4l-utils))

(in-package lifoo)

(defvar *lifoo* nil)
(defvar *lifoo-init* (make-hash-table :test 'equal))

(defmacro define-init (tags &body body)
  "Defines init for TAGS around BODY"
  `(setf (gethash ',tags *lifoo-init*)
         (lambda (exec)
           (with-lifoo (:exec exec)
             ,@body))))

(defmacro define-macro-word (name (in &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define-macro (keyword! ',name)
                       (lambda (,in)
                         ,@body)
                       :exec (or ,exec *lifoo*)))

(defmacro define-lisp-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  (let ((id (keyword! name)))
    `(lifoo-define ,id
                   (make-lifoo-word :id ,id
                                    :source ',body
                                    :fn (lambda () ,@body))
                   :exec (or ,exec *lifoo*))))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ',name
                 (make-lifoo-word :id ,(keyword! name)
                                  :source ',body)
                 :exec (or ,exec *lifoo*)))

(defmacro define-binary-words ((&key exec) &rest forms)
  "Defines new words in EXEC for FORMS"
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) (:exec ,exec)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 forms))))

(defmacro do-lifoo ((&key (env t) exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo* (make-lifoo))
                :env ,env)
     (lifoo-eval ',body)
     (lifoo-pop)))

(defmacro with-lifoo ((&key env exec) &body body)
  "Runs body with *LIFOO* bound to EXEC or new; environment
   is bound to ENV if not NIL, or copy of current if T"
  `(let ((*lifoo* (or ,exec (make-lifoo))))
     (when ,env (lifoo-begin :env ,env))
     (unwind-protect (progn ,@body)
       (when ,env (lifoo-end)))))

(defstruct (lifoo-word (:conc-name))
  id
  trace?
  source fn)

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  envs logs
  (stack (make-array 3 :adjustable t :fill-pointer 0))
  (set-stack (make-array 3 :adjustable t :fill-pointer 0))
  (macro-words (make-hash-table :test 'eq))
  (words (make-hash-table :test 'eq)))

(define-condition lifoo-break (condition) ()) 

(define-condition lifoo-error (simple-error) ()) 

(define-condition lifoo-throw (condition)
  ((value :initarg :value :reader value)))

(defun lifoo-break ()
  "Signals break"
  (signal 'lifoo-break))

(defun lifoo-error (fmt &rest args)
  "Signals error with message from FMT and ARGS"
  (error 'lifoo-error :format-control fmt :format-arguments args))

(defun lifoo-throw (val)
  "Throws VAL"
  (signal 'lifoo-throw :value val))

(defun lifoo-init (tags &key (exec *lifoo*))
  "Runs all inits matching tags in EXEC"
  (let ((cnt 0))
    (do-hash-table (ts fn *lifoo-init*)
      (when (or (eq t tags)
                (null (set-difference ts tags)))
        (funcall fn exec)
        (incf cnt)))
    (assert (not (zerop cnt))))
  exec)

(defun lifoo-read (&key (in *standard-input*))
  "Reads Lifoo code from IN until end of file"
  (let ((eof? (gensym)) (more?) (expr))
    (do-while ((not (eq (setf more? (read in nil eof?)) eof?)))
      (push more? expr))
    (nreverse expr)))

(defun lifoo-parse (expr &key (exec *lifoo*))
  "Parses EXPR and returns code compiled for EXEC"
  (labels
      ((parse (fs acc)
         (if fs
             (let ((f (first fs)))
               (parse
                (rest fs)
                (cond
                  ((simple-vector-p f)
                   (let ((len (length f)))
                     (cons (cons f `(lifoo-push
                                     ,(make-array
                                       len
                                       :adjustable t
                                       :fill-pointer len
                                       :initial-contents f)))
                           acc)))
                  ((keywordp f)
                   (cons (cons f `(lifoo-push ,f)) acc))
                  ((consp f)
                   (if (consp (rest f))
                       (cons (cons f `(lifoo-push ',(copy-list f)))
                             acc)
                       (cons (cons f `(lifoo-push
                                       ',(cons (first f)
                                               (rest f))))
                             acc)))
                  ((null f)
                   (cons (cons f `(lifoo-push nil)) acc))
                  ((eq f t)
                   (cons (cons f `(lifoo-push t)) acc))
                  ((symbolp f)
                   (let* ((id (keyword! f))
                          (mw (lifoo-macro-word id)))
                     (if mw
                         (funcall mw acc)
                         (cons (cons f `(lifoo-call ,id)) acc))))
                  ((lifoo-word-p f)
                   (cons (cons f `(lifoo-call ,f)) acc))
                  (t
                   (cons (cons f `(lifoo-push ,f)) acc)))))
             (mapcar #'rest (nreverse acc)))))
    (with-lifoo (:exec exec)
      (parse (list! expr) nil))))

(defun lifoo-eval (expr &key (exec *lifoo*))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (handler-case
        (eval `(progn ,@(lifoo-parse expr)))
      (lifoo-throw (c)
        (lifoo-error "thrown value not caught: ~a" (value c))))))

(defun lifoo-compile (word &key (exec *lifoo*))
  "Returns compiled function for WORD"
  (or (fn word)
      (setf (fn word)
            (eval `(lambda ()
                     ,@(lifoo-parse (source word) :exec exec))))))

(defun lifoo-call (word &key (exec *lifoo*))
  "Calls WORD in EXEC"

  (unless (lifoo-word-p word)
    (let ((id word))
      (unless (setf word (lifoo-word id))
        (error "missing word: ~a" id)))) 

  (when (trace? word)
    (push (list :enter (id word) (clone (stack exec)))
          (logs exec)))

  (with-lifoo (:exec exec)
    (handler-case
        (progn 
          (funcall (lifoo-compile word))

          (when (trace? word)
            (push (list :exit (id word) (clone (stack exec)))
                  (logs exec))))
      (lifoo-break ()
        (when (trace? word)
          (push (list :break (id word) (clone (stack exec)))
                (logs exec)))))))

(defun lifoo-macro-word (id &key (exec *lifoo*))
  "Returns macro word for ID from EXEC, or NIL if missing"
  (gethash (keyword! id) (macro-words exec)))

(defun lifoo-define-macro (id word &key (exec *lifoo*))
  "Defines ID as macro WORD in EXEC"
  (setf (gethash (keyword! id) (macro-words exec)) word))

(defun lifoo-define (id word &key (exec *lifoo*))
  "Defines ID as WORD in EXEC"
  (setf (gethash (keyword! id) (words exec)) word))

(defun lifoo-undefine (word &key (exec *lifoo*))
  "Undefines word for ID in EXEC"
  (remhash (words exec) (if (lifoo-word-p word)
                            (id word)
                            (keyword! word))))

(defun lifoo-word (id &key (exec *lifoo*))
  "Returns word for ID from EXEC, or NIL if missing"
  (if (lifoo-word-p id)
      id
      (gethash (keyword! id) (words exec))))

(defun lifoo-push (val &key (exec *lifoo*) set)
  "Pushes VAL onto EXEC stack"  
  (vector-push-extend val (stack exec))
  (vector-push-extend set (set-stack exec))
  val)

(defmacro lifoo-push-set (expr &key exec)
  `(lifoo-push ,expr
               :set (lambda (val)
                      (setf ,expr val))
               :exec (or ,exec *lifoo*)))

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops and returns value from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (vector-pop (set-stack exec))
    (vector-pop (stack exec))))

(defun lifoo-peek (&key (exec *lifoo*))
  "Returns top of EXEC stack"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (unless (zerop fp)
      (aref stack (1- fp)))))

(defun lifoo-peek-set (&key (exec *lifoo*))
  "Returns top of EXEC set-stack"
  (let* ((stack (set-stack exec))
         (fp (fill-pointer stack)))
    (unless (zerop fp)
      (aref stack (1- fp)))))

(defun (setf lifoo-peek) (val &key (exec *lifoo*))
  "Replaces top of EXEC stack with VAL"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (assert (not (zerop fp)))
    (setf (aref stack (1- fp)) val)))

(defun lifoo-reset (&key (exec *lifoo*))
  "Resets EXEC stack"
  (setf (fill-pointer (stack exec)) 0)
  (setf (fill-pointer (set-stack exec)) 0))

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
  "Prints log to OUT"
  (dolist (e log)
    (apply #'format out
           (ecase (first e)
             (:break "BREAK ~a ~a~%")
             (:enter "ENTER ~a ~a~%")
             (:exit  "EXIT  ~a ~a~%")
             (:log   "LOG   ~a~%"))
           (rest e))))

(defun lifoo-stack (&key (exec *lifoo*))
  "Returns stack for EXEC"
  (stack exec))

(defun lifoo-begin (&key (env t) (exec *lifoo*))
  "Opens ENV or new environment if T in EXEC"
  (push (if (eq t env) (copy-list (lifoo-env)) env)
        (envs exec)))

(defun lifoo-end (&key (exec *lifoo*))
  "Closes current environment in EXEC"
  (pop (envs exec)))

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

(defun lifoo-repl (&key (exec (lifoo-init t :exec (make-lifoo)))
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
                   (lifoo-reset)
                   (lifoo-eval (lifoo-read :in in))
                   (format out "~a~%" (lifoo-pop)))
               (ignore ()
                 :report "Ignore error and continue.")))
           (go start))))))
