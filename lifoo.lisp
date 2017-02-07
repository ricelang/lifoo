(defpackage lifoo
  (:export define-binary-words define-lifoo-init
           define-lifoo-struct
           define-lifoo-struct-fn define-lisp-word define-word
           do-lifoo do-lifoo-call
           lifoo-call lifoo-compile
           lifoo-compile-fn lifoo-compile-form lifoo-compile-word
           lifoo-del lifoo-define lifoo-define-macro lifoo-dump-log
           lifoo-env lifoo-error lifoo-eval
           lifoo-init lifoo-log
           lifoo-optimize
           lifoo-peek lifoo-peek-set
           lifoo-pop lifoo-print-log lifoo-push
           lifoo-read lifoo-repl lifoo-reset
           lifoo-set lifoo-stack lifoo-stream
           lifoo-trace?
           lifoo-undefine
           lifoo-var lifoo-word lifoo-write make-lifoo
           with-lifoo
           *lifoo* *lifoo-init* *lifoo-speed*)
  (:use bordeaux-threads cl cl4l-chan cl4l-clone cl4l-compare
        cl4l-crypt cl4l-index cl4l-test cl4l-utils))

(in-package lifoo)

(defparameter *lifoo-speed* 3)

(defvar *lifoo* nil)
(defvar *lifoo-init* (index nil))

(defmacro define-lifoo-init (tags &body body)
  "Defines init for TAGS around BODY"
  `(progn
     (index-remove *lifoo-init* ',tags)
     (index-add *lifoo-init*  
                (lambda (exec)
                  (with-lifoo (:exec exec)
                    ,@body))
                :key ',tags)))

(defmacro define-macro-word (id (in out &key exec)
                             &body body)
  "Defines new macro word NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,id (make-lifoo-word
                      :id ,id
                      :macro? t
                      :source ',body
                      :fn (lambda (,in ,out)
                            ,@body))
                 :exec (or ,exec *lifoo*)))

(defmacro lifoo-optimize (&key speed)
  `(let ((spd (or ,speed *lifoo-speed*)))
     `(declare (optimize (speed ,spd)
                         (safety ,(- 3 spd))))))

(defmacro define-lisp-word (id (&key exec speed) &body body)
  "Defines new word with NAME in EXEC from Lisp forms in BODY"
  `(lifoo-define ,id
                 (make-lifoo-word
                  :id ,id
                  :source ',body
                  :fn (lambda ()
                        ,(lifoo-optimize :speed speed)
                        ,@body))
                 :exec (or ,exec *lifoo*)))

(defmacro define-word (name (&key exec) &body body)
  "Defines new word with NAME in EXEC from BODY"
  `(lifoo-define ',name
                 (make-lifoo-word :id ,(keyword! name)
                                  :source ',body)
                 :exec (or ,exec *lifoo*)))

(defmacro define-binary-words ((&key exec speed) &rest forms)
  "Defines new words in EXEC for FORMS"
  (with-symbols (_lhs _rhs)
    `(progn
       ,@(mapcar (lambda (op)
                   `(define-lisp-word ,(keyword! op)
                        (:exec ,exec :speed ,speed)
                      (let ((,_lhs (lifoo-pop))
                            (,_rhs (lifoo-pop)))
                        (lifoo-push (,op ,_lhs ,_rhs)))))
                 forms))))

(defmacro do-lifoo ((&key (env t) exec) &body body)
  "Runs BODY in EXEC"
  `(with-lifoo (:exec (or ,exec *lifoo* (make-lifoo))
                :env ,env)
     (lifoo-eval ',body :throw? nil)
     (lifoo-pop)))

(defmacro do-lifoo-call ((word &key exec) &body body)
  (with-symbols (_w)
    `(with-lifoo (:exec (or ,exec *lifoo*))
       (let ((,_w ,word))
         (when (trace? ,_w)
           (push (list :enter (id ,_w) (lifoo-stack))
                 (logs *lifoo*)))
         ,@body

         (when (trace? ,_w)
           (push (list :exit (id ,_w) (lifoo-stack))
                 (logs *lifoo*)))))))

(defmacro with-lifoo ((&key env exec) &body body)
  "Runs body with *LIFOO* bound to EXEC or new; environment
   is bound to ENV if not NIL, or copy of current if T"
  `(let ((*lifoo* (or ,exec (make-lifoo))))
     (when ,env (lifoo-begin :env ,env))
     (unwind-protect (progn ,@body)
       (when ,env (lifoo-end)))))

(defmacro define-lifoo-struct (name fields)
  "Defines struct NAME with FIELDS"
  `(progn
     (let ((lisp-name (gensym))
           (fs ,fields))
       (eval `(defstruct (,lisp-name)
                ,@fs))
       (define-lifoo-struct-fn
           (keyword! 'make- ,name) (symbol! 'make- lisp-name)
         (lifoo-pop))
       (define-lifoo-struct-fn
           (keyword! ,name '?) (symbol! lisp-name '-p)
         (list (lifoo-peek)))
       (dolist (f fs)
         (let ((fn (if (consp f) (first f) f)))
           (define-lifoo-struct-fn
               (keyword! ,name '- fn) (symbol! lisp-name '- fn)
             (list (lifoo-peek)) :set? t))))))

(defmacro define-lifoo-struct-fn (lifoo lisp args &key set?)
  "Defines word LIFOO that calls LISP with ARGS"
  (with-symbols (_fn _sfn)
    `(let ((,_fn (symbol-function ,lisp))
           (,_sfn (and ,set? (fdefinition (list 'setf ,lisp)))))
       
       (define-lisp-word ,lifoo ()
         (lifoo-push
          (apply ,_fn ,args)
          :set (when ,set?
                 (lambda (val)
                   (lifoo-pop)
                   (funcall ,_sfn val (lifoo-peek)))))))))

(defstruct (lifoo-word (:conc-name))
  id macro? trace? source fn)

(defstruct (lifoo-cell (:conc-name lifoo-))
  val set del)

(defstruct (lifoo-exec (:conc-name)
                       (:constructor make-lifoo))
  (envs (list (index nil))) logs
  (backup-key (gensym)) (defer-key (gensym)) (stream-key (gensym))
  (stack (make-array 3 :adjustable t :fill-pointer 0))
  (words (index nil)))

(define-condition lifoo-error (simple-error) ()) 

(define-condition lifoo-throw (condition)
  ((value :initarg :value :reader value)))

(defun lifoo-error (fmt &rest args)
  "Signals error with message from FMT and ARGS"
  (error 'lifoo-error :format-control fmt :format-arguments args))

(defun lifoo-throw (val)
  "Throws VAL"
  (signal 'lifoo-throw :value val))

(defun lifoo-init (tags &key (exec *lifoo*))
  "Runs all inits matching tags in EXEC"
  (let ((cnt 0))
    (do-index (ts fn *lifoo-init*)
      (when (or (eq t tags)
                (null (set-difference ts tags)))
        (funcall fn exec)
        (incf cnt)))
    (when (zerop cnt)
      (error "init not found: ~a" tags)))
  exec)

(defun lifoo-stream (stream &key (exec *lifoo*))
  (push stream (lifoo-var (stream-key exec))))

(defun lifoo-read (&key (in *standard-input*))
  "Reads Lifoo code from IN until end of file"
  (let ((eof? (gensym)) (more?) (expr))
    (handler-case
     (do-while ((not (eq (setf more? (read in nil eof?)) eof?)))
       (push more? expr))
     (end-of-file ()))
    (nreverse expr)))

(defun lifoo-write (expr &key (out *standard-output*))
  (let ((sep))
    (dolist (f expr)
      (when sep (princ sep out))
      (write f :stream out)
      (setf sep #\space))))

(defun lifoo-compile-fn (expr &key (exec *lifoo*) speed)
  (eval `(lambda ()
           ,(lifoo-optimize :speed speed)
           ,@(lifoo-compile expr :exec exec))))

(defun lifoo-expand (in out)
  "Expands IN into OUT and returns new token stream"
  (let ((word (lifoo-word in)))
    (if word
        (if (macro? word)
            (let ((exp (funcall (fn word) in out)))
              (cons (cons (first (first exp)) 
                          `(do-lifoo-call ((lifoo-word ',in))
                               ,(rest (first exp))))
               (rest exp)))
            (progn
              (cons (cons in `(lifoo-call ',in)) out)))
        (cons (cons in `(lifoo-call ',in))
              out))))

(defun lifoo-compile-form (f in)
  "Compiles form F for token stream IN and returns new stream"
  (cond
    ((simple-vector-p f)
     (let ((len (length f)))
       (cons (cons f `(lifoo-push
                          ,(make-array
                            len
                            :adjustable t
                            :fill-pointer len
                            :initial-contents f)))
             in)))
    
    ((consp f)
     (if (consp (rest f))
         (cons (cons f `(lifoo-push ',(copy-list f))) in)
         (cons (cons f `(lifoo-push ',(cons (first f) (rest f))))
               in)))
    ((null f)
     (cons (cons f `(lifoo-push nil)) in))
    ((eq f t)
     (cons (cons f `(lifoo-push t)) in))
    ((or (and (symbolp f) (not (keywordp f)))
         (lifoo-word-p f))
     (lifoo-expand f in))
    ((functionp f)
     (cons (cons f `(funcall ,f)) in))
    (t
     (cons (cons f `(lifoo-push ,f)) in))))

(defun lifoo-compile (forms &key (exec *lifoo*))
  "Parses EXPR and returns code for EXEC"
  (labels ((compile-forms (in out)
             (if in
                 (let ((f (first in)))
                   (compile-forms (rest in)
                                  (lifoo-compile-form f out)))
                 (mapcar #'rest (nreverse out)))))
    (with-lifoo (:exec exec)
      (compile-forms (list! forms) nil))))

(defun lifoo-eval (expr &key (exec *lifoo*) (throw? t))
  "Returns result of parsing and evaluating EXPR in EXEC"
  (with-lifoo (:exec exec)
    (let ((code `(progn ,@(lifoo-compile expr))))
      (if throw?
          (eval code)
          (handler-case
              (eval code)
            (lifoo-throw (c)
              (lifoo-error "thrown value not caught: ~a"
                           (value c))))))))

(defun lifoo-compile-word (word &key (exec *lifoo*) speed)
  "Makes sure word is compiled and returns function"
  (or (fn word)
      (setf (fn word)
            (eval `(lambda ()
                     ,(lifoo-optimize :speed speed)
                     ,@(lifoo-compile (source word)
                                      :exec exec))))))

(defun lifoo-call (word &key (exec *lifoo*))
  "Calls WORD in EXEC"

  (unless (lifoo-word-p word)
    (let ((id word))
      (unless (setf word (lifoo-word id))
        (error "missing word: ~a" id)))) 

  (do-lifoo-call (word :exec exec)
    (funcall (lifoo-compile-word word))))

(defun lifoo-define (id word &key (exec *lifoo*))
  "Defines ID as WORD in EXEC"
  (lifoo-undefine id :exec exec)
  (index-add (words exec) word :key (keyword! id)))

(defun lifoo-undefine (word &key (exec *lifoo*))
  "Undefines word for WORD in EXEC"
  (index-remove (words exec) (if (lifoo-word-p word)
                                 (id word)
                                 (keyword! word))))

(defun lifoo-word (word &key (exec *lifoo*))
  "Returns WORD from EXEC, or NIL if missing"
  (if (lifoo-word-p word)
      word
      (index-find (words exec) (keyword! word))))

(defun lifoo-push-cell (cell &key (exec *lifoo*))
  "Pushes CELL onto EXEC stack"  
  (vector-push-extend cell (stack exec))
  cell)

(defun lifoo-push (val &key (exec *lifoo*) set del)
  "Pushes VAL onto EXEC stack"  
  (lifoo-push-cell (make-lifoo-cell :val val :set set :del del)
                   :exec exec)
  val)

(defmacro lifoo-push-expr (expr &key del exec)
  `(lifoo-push ,expr
               :set (lambda (val)
                      (setf ,expr val))
               :del ,(when del
                       `(lambda () ,del))
               :exec (or ,exec *lifoo*)))

(defun lifoo-pop-cell (&key (exec *lifoo*))
  "Pops cell from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (vector-pop (stack exec))))

(defun lifoo-pop (&key (exec *lifoo*))
  "Pops value from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (lifoo-val (lifoo-pop-cell :exec exec))))

(defun lifoo-peek-cell (&key (exec *lifoo*))
  "Returns top cell from EXEC stack"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (unless (zerop fp)
      (aref stack (1- fp)))))

(defun lifoo-peek (&key (exec *lifoo*))
  "Returns top value from EXEC stack"
  (unless (zerop (fill-pointer (stack exec)))
    (lifoo-val (lifoo-peek-cell :exec exec))))

(defun (setf lifoo-peek) (val &key (exec *lifoo*))
  "Replaces top of EXEC stack with VAL"
  (let* ((stack (stack exec))
         (fp (fill-pointer stack)))
    (assert (not (zerop fp)))
    (setf (lifoo-val (aref stack (1- fp))) val)))

(defun lifoo-dup (&key (exec *lifoo*))
  "Pushes top of stack on stack in EXEC"
  (lifoo-push-cell (lifoo-peek-cell :exec exec) :exec exec))

(defun lifoo-swap (&key (exec *lifoo*))
  "Swaps top and previous items in EXEC stack"
  (let* ((stack (stack exec))
         (pos (1- (fill-pointer stack)))
         (tmp (aref stack (1- pos))))
    (setf (aref stack (1- pos)) (aref stack pos))
    (setf (aref stack pos) tmp)))

(defun lifoo-reset (&key (exec *lifoo*))
  "Resets EXEC stack"
  (setf (fill-pointer (stack exec)) 0))

(defun lifoo-backup (&key (exec *lifoo*))
  "Stores backup of EXEC stack in current environment"
  (push (copy-seq (stack exec)) (lifoo-var (backup-key exec))))

(defun lifoo-restore (&key (exec *lifoo*))
  "Restores EXEC stack from current environment"
  (let* ((prev (pop (lifoo-var (backup-key exec))))
         (curr (stack exec))
         (len (length prev)))
    (setf (fill-pointer curr) len)
    (dotimes (i len)
      (setf (aref curr i) (aref prev i)))))

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
  (nreverse (map 'list #'lifoo-val (stack exec))))

(defun lifoo-begin (&key (env t) (exec *lifoo*))
  "Opens ENV or new environment if T in EXEC"
  (push (if (eq t env) (index-copy (lifoo-env)) env)
        (envs exec)))

(defun lifoo-end (&key (exec *lifoo*) (throw? t))
  "Closes current environment in EXEC"
  (dolist (fn (lifoo-var (defer-key exec)))
    (lifoo-eval fn :exec exec :throw? throw?))
  (dolist (stm (lifoo-var (stream-key exec)))
    (close stm))
  (pop (envs exec)))

(defun lifoo-env (&key (exec *lifoo*))
  "Returns current environment"
  (first (envs exec)))

(defun (setf lifoo-env) (env &key (exec *lifoo*))
  "Replaces current environment"
  (if (envs exec)
      (rplaca (envs exec) env)
      (when env
        (error "setf nil environment"))))

(defun lifoo-var (var &key (exec *lifoo*))
  "Returns value of VAR in EXEC"
  (index-find (lifoo-env :exec exec) var)) 

(defun (setf lifoo-var) (val var &key (exec *lifoo*))
  "Sets value of VAR in EXEC to VAL"
  (index-remove (lifoo-env :exec exec) var)
  (index-add (lifoo-env :exec exec) val :key var)
  val)

(defun lifoo-repl (&key (exec (lifoo-init t :exec (make-lifoo)))
                        (in *standard-input*)
                        (out *standard-output*))
  "Starts a REPL for EXEC with input from IN and output to OUT,
   using PROMPT"

  (format out "Welcome to Lifoo,~%")
  (format out "press enter on empty line to evaluate,~%")
  (format out "exit ends session~%")
  
  (with-lifoo (:exec exec :env t)
    (tagbody
     start
       (format out "~%Lifoo> ")
       (let ((expr (with-output-to-string (expr)
                      (let ((line))
                        (do-while
                            ((not
                              (string= "" (setf line
                                                (read-line in
                                                           nil)))))
                          (when (string= "exit" line) (go end))
                          (terpri expr)
                          (princ line expr))))))
         (with-input-from-string (in expr)
           (restart-case
               (progn
                 (lifoo-reset)
                 (lifoo-eval (lifoo-read :in in) :throw? nil)
                 (write (lifoo-pop) :stream out)
                 (terpri out))
             (ignore ()
               :report "Ignore error and continue.")))
         (go start))
     end)))
