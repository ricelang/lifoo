(defpackage lifoo
  (:export define-init define-binary-words define-lisp-word
           define-word do-lifoo
           lifoo-call lifoo-define
           lifoo-del lifoo-dump-log
           lifoo-env lifoo-error lifoo-eval
           lifoo-fn lifoo-get
           lifoo-init lifoo-log
           lifoo-parse lifoo-parse-word lifoo-pop lifoo-print-log
           lifoo-push
           lifoo-read lifoo-repl
           lifoo-stack
           lifoo-trace?
           lifoo-undefine
           lifoo-word make-lifoo
           with-lifoo
           *lifoo*)
  (:use bordeaux-threads cl cl4l-chan cl4l-clone cl4l-compare
        cl4l-test cl4l-utils))

(in-package lifoo)

(defvar *lifoo* nil)

(defmacro define-lisp-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (make-lifoo-word :id ,(keyword! name)
                                    :source ',body
                                    :fn (lambda () ,@body)))))

(defmacro define-binary-words ((&key exec) &rest forms)
  "Defines new words in EXEC for FORMS"
  (with-symbols (_lhs _rhs)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op) (:exec ,exec)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 forms))))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(with-lifoo (:exec (or ,exec *lifoo*))
     (lifoo-define ',name
                   (make-lifoo-word :id ,(keyword! name)
                                    :source ',body))))

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
  (words (make-hash-table :test 'eq)))

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
      ((rec (forms acc)
         (if forms
             (let ((f (first forms)))
               (cond
                 ((or (arrayp f)
                      (characterp f)
                      (keywordp f)
                      (numberp f)
                      (stringp f))
                  (rec (rest forms) (cons `(lifoo-push ,f) acc)))
                 ((consp f)
                  (rec (rest forms) (cons `(lifoo-push ',f) acc)))
                 ((null f)
                  (rec (rest forms) (cons `(lifoo-push nil) acc)))
                 ((eq f t)
                  (rec (rest forms) (cons `(lifoo-push t) acc)))
                 ((symbolp f)
                  (rec (rest forms) (cons `(lifoo-call ',f) acc)))
                 ((lifoo-word-p f)
                  (rec (rest forms) (cons `(lifoo-call ,f) acc)))
                 (t
                  (error "invalid form: ~a" f))))
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
  (unless (lifoo-word-p word)
    (unless (setf word (lifoo-word word))
      (error "missing word: ~a" word))) 
  
  (when (trace? word)
    (push (list :enter (id word) (clone (stack exec)))
          (logs exec)))

  (with-lifoo (:exec exec)
    (funcall (lifoo-fn word)))

  (when (trace? word)
    (push (list :exit (id word) (clone (stack exec)))
          (logs exec))))

(defun lifoo-define (id word &key (exec *lifoo*))
  "Defines ID as WORD in EXEC"
  (setf (gethash (keyword! id) (words exec)) word))

(defun lifoo-undefine (word &key (exec *lifoo*))
  "Undefines word for ID in EXEC"
  (remhash (words exec) (if (lifoo-word-p word)
                            (id word)
                            (keyword! word))))

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
                     (lifoo-eval (lifoo-read :in in))
                     (format out "~a~%" (lifoo-pop)))
                 (ignore ()
                   :report "Ignore error and continue.")))
             (go start))))))
