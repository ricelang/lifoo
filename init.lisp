(in-package lifoo)

(define-init (:abc)
  (define-binary-words (:speed 1) + - * / = /= < > cons)

  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? (nil)
    (lifoo-push (null (lifoo-pop))))

  ;; Pushes clone of $1
  (define-lisp-word :clone (nil)
    (lifoo-push (clone (lifoo-peek))))

  ;; Pops $val and pushes its symbolic representation
  (define-lisp-word :symbol (nil)
    (lifoo-push (keyword! (lifoo-pop))))

  ;; Increases $1
  (define-lisp-word :inc (nil :speed 1)
    (incf (lifoo-peek)))

  ;; Decreases $1
  (define-lisp-word :dec (nil :speed 1)
    (decf (lifoo-peek)))

  ;; Pops $val and sets value of $1 to $val
  (define-lisp-word :set (nil :speed 1)
    (let* ((val (lifoo-pop))
           (cell (lifoo-peek-cell))
           (set (lifoo-set cell)))
      (unless set
        (error "missing set: ~a" val))
      (funcall set val)))

  ;; Pushes and deletes value of $1
  (define-lisp-word :del (nil :speed 1)
    (let* ((cell (lifoo-peek-cell))
           (val (lifoo-val cell))
           (del (lifoo-del cell)))
      (unless del
        (error "missing del: ~a" val))
      (funcall del)
      (lifoo-push val)))

  ;; Pops $expr,
  ;; measures the time it takes to run;
  ;; and pushes cpu and real
  (define-lisp-word :time ((t) :speed 1)
    (let* ((fn (lifoo-pop))
           (real (get-internal-real-time))
           (cpu (get-internal-run-time)))
      (funcall fn)
      (lifoo-push (- (get-internal-run-time) cpu))
      (lifoo-push (- (get-internal-real-time) real))))
  
  ;; Pops $fields and $name,
  ;; and defines struct
  (define-lisp-word :struct (nil)
    (let ((name (lifoo-pop))
          (fields (lifoo-pop)))
      (define-lifoo-struct name fields)))
  
  ;; Pops $str and pushes read result
  (define-lisp-word :read (nil)
    (with-input-from-string (in (lifoo-pop))
      (lifoo-push (lifoo-read :in in))))
  
  ;; Pops $expr and pushes string representation
  (define-lisp-word :write (nil)
    (lifoo-push
     (with-output-to-string (out)
       (lifoo-write (lifoo-pop) :out out))))

  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval (nil)
    (lifoo-eval (lifoo-pop))))

(define-init (:array)
  ;; Pops $items and pushes new array
  (define-lisp-word :array (nil :speed 1)
    (let* ((items (lifoo-pop))
           (len (length items)))
      (lifoo-push (make-array len
                              :initial-contents items
                              :adjustable t
                              :fill-pointer len)))))

(define-init (:compare)
  ;; Pops $lhs and $rhs,
  ;; and pushes result of comparing $lhs to $rhs
  (define-lisp-word :cmp (nil)
    (let ((lhs (lifoo-pop))
          (rhs (lifoo-pop)))
      (lifoo-push (compare lhs rhs))))

  ;; Pops $rhs and $lhs,
  ;; and pushes T if they compare equal
  (define-word :eq? (nil) cmp 0 =)
  (define-word :neq? (nil) cmp 0 /=)
  (define-word :lt? (nil) cmp -1 =)
  (define-word :gt? (nil) cmp 1 =)
  (define-word :lte? (nil) cmp 1 <)
  (define-word :gte? (nil) cmp -1 >))

(define-init (:env)
  ;; Pushes current environment on stack
  (define-lisp-word :env (nil)
    (lifoo-push (lifoo-env)))

  ;; Pops $var and returns value
  (define-lisp-word :var (nil)
    (lifoo-eval (lifoo-pop))
    (let ((var (lifoo-pop)))
      (lifoo-push-expr (lifoo-var var)
                       :del (setf (lifoo-var var) nil))))

  ;; Opens new environment
  (define-lisp-word :begin (nil)
    (lifoo-begin))

  ;; Closes current environment
  (define-lisp-word :end (nil)
    (lifoo-end))

  ;; Clears current environment
  (define-lisp-word :clear (nil)
    (setf (lifoo-env) nil)))

(define-init (:error)
  ;; Pops $cnd and signals error if NIL
  (define-macro-word :assert (in out)
    (cons (cons in
                `(let ((ok? (progn
                              ,@(lifoo-compile (first (first out)))
                              (lifoo-pop))))
                   (unless ok?
                     (lifoo-error "assert failed: ~a"
                                  ',(first (first out))))))
          (rest out)))

  ;; Pops $expected and $actual,
  ;; and signals error they don't compare equal  
  (define-macro-word :asseq (in out)
    (cons (cons in
                `(let ((expected (progn
                                   ,@(lifoo-compile
                                      (first (first out)))))
                       (actual (progn
                                 ,@(lifoo-compile
                                    (first (second out))))))
                   (unless (zerop (compare expected actual))
                     (lifoo-error "assert failed: ~a /= ~a"
                                  actual expected))))
          (nthcdr 2 out))))

(define-init (:flow)
  ;; Pops $cnd, $true and $false;
  ;; and pushes $true if $cnd, otherwise $false
  (define-macro-word :cond (in out)
    (cons (cons in
                `(progn
                   ,@(lifoo-compile (first (first out)))
                   (if (lifoo-pop)
                       (progn ,@(lifoo-compile
                                 (first (second out))))
                       (progn ,@(lifoo-compile
                                 (first (third out)))))))
          (nthcdr 3 out)))
  
  ;; Pops $cnd and $res;
  ;; and pushes $res if $cnd, otherwise NIL
  (define-macro-word :when (in out)
    (cons (cons in
                `(progn
                   ,@(lifoo-compile (first (first out)))
                   (if (lifoo-pop)
                       (progn
                         ,@(lifoo-compile (first (second out))))
                       (lifoo-push nil))))
          (nthcdr 2 out)))

  ;; Pops $cnd and $res;
  ;; and pushes $res unless $cnd, otherwise NIL
  (define-macro-word :unless (in out)
    (cons (cons in
                `(progn
                   ,@(lifoo-compile (first (first out)))
                   (if (lifoo-pop)
                       (lifoo-push nil)
                       (progn
                         ,@(lifoo-compile (first (second out)))))))
          (nthcdr 2 out)))

  ;; Pops $reps and $body;
  ;; and repeats $body $reps times,
  ;; pushing indexes before evaluating body
  (define-macro-word :times (in out)
    (cons (cons in `(let ((reps (lifoo-pop)))
                      (dotimes (i reps)
                        (lifoo-push i)
                        ,@(lifoo-compile (first (second out))))))
     (cons (first out) (rest (rest out)))))

  ;; Pops $body and loops until $body pushes nil 
  (define-macro-word :while (in out)
    (cons
     (cons in
           `(progn
              (do-while ((progn
                           ,@(lifoo-compile (first (first out)))
                           (lifoo-peek)))
                (lifoo-pop))
              (lifoo-pop)))
     (rest out)))

  ;; Pops $value and throws it 
  (define-lisp-word :throw (nil)
    (lifoo-throw (lifoo-pop)))

  ;; Wraps parsed forms in unwind-protect with previous
  ;; form as body
  (define-macro-word :always (in out)
    (list
     (cons in `(unwind-protect
                    (progn
                      ,@(reverse (mapcar #'rest (rest out))))
                 (lifoo-eval ',(first (first out)))))))
  
  ;; Wraps parsed forms in handler-case with previous
  ;; form as handler
  (define-macro-word :catch (in out)
    (list
     (cons in `(handler-case
                   (progn
                     ,@(reverse (mapcar #'rest (rest out))))
                 (lifoo-throw (c)
                   (lifoo-push (value c))
                   (lifoo-eval ',(first (first out)))))))))

(define-init (:io)
  ;; Pops $val and prints it
  (define-lisp-word :print (nil)
    (princ (lifoo-pop)))

  ;; Prints line ending
  (define-lisp-word :ln (nil)
    (terpri)))

(define-init (:list)
  (define-binary-words () cons)

  ;; Pushes stack as list and clears stack
  (define-lisp-word :list (nil)
    (let ((lst (map 'list #'identity (lifoo-pop))))
      (lifoo-push lst)))

  ;; Pops $list and pushes rest
  (define-lisp-word :rest (nil)
    (let ((lst (lifoo-pop)))
      (lifoo-push (rest lst)
                  :set (lambda (val)
                         (lifoo-pop)
                         (setf (rest lst) val)
                         (lifoo-push lst)))))

  ;; Pops $list and pushes first element
  (define-lisp-word :first (nil)
    (let ((lst (lifoo-pop)))
      (lifoo-push (first lst)
                  :set (lambda (val)
                         (lifoo-pop)
                         (lifoo-push (rplaca lst val)))))))

(define-init (:log)
  ;; Pops $word and enables tracing
  (define-lisp-word :trace (nil)
    (setf (trace? (lifoo-word (lifoo-pop))) t))

  ;; Pops $word and disabled tracing,
  ;; T disables all
  (define-lisp-word :untrace (nil)
    (let ((expr (lifoo-pop)))
      (if (eq t expr)
          (do-hash-table (_ w (words *lifoo*))
            (setf (trace? w) nil))
          (setf (trace? (lifoo-word expr)) nil))))

  ;; Pops $msg and logs it
  (define-lisp-word :log (nil)
    (lifoo-log (lifoo-pop)))

  ;; Dumps log on stack
  (define-lisp-word :dump-log (nil)
    (lifoo-push (lifoo-dump-log)))

  ;; Prints log
  (define-lisp-word :print-log (nil)
    (lifoo-print-log (lifoo-dump-log))))

(define-init (:meta)
  ;; Pops $protos and initialises protocols
  (define-lisp-word :init (nil)
    (let ((protos (lifoo-pop)))
      (lifoo-init (if (consp protos) protos (list protos)))))

  ;; Pops $code and pushes compiled code
  (define-lisp-word :compile (nil)
    (let ((code (lifoo-pop)))
      (lifoo-push (cons 'progn
                        (lifoo-compile code)))))
  
  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp (nil)
    (let ((expr (lifoo-pop)))
      (lifoo-push (make-lifoo-word
                   :source expr
                   :fn (eval `(lambda () ,expr)))))))

(define-init (:sequence)
  ;; Pops $idx and pushes item from seq
  (define-lisp-word :nth (nil :speed 1)
    (let* ((idx (lifoo-pop))
           (seq (lifoo-peek))
           (it (elt seq idx)))
      (lifoo-push
       it
       :set (lambda (val)
              (setf (elt seq idx) val))
       :del (lambda ()
              (lifoo-pop)
              (lifoo-push (remove-nth
                           idx
                           seq))))))
  
  ;; Pushes length of $1
  (define-lisp-word :length (nil :speed 1)
    (let ((val (lifoo-peek)))
      (lifoo-push
       (cond
         ((chan? val)
          (chan-length val))
         (t
          (length val))))))
  
  ;; Pops item from seq in $1 and pushes it
  (define-lisp-word :pop (nil)
    (let* ((seq (lifoo-peek))
           (it (cond
                 ((arrayp seq)
                  (vector-pop seq))
                 (t
                  (pop seq)))))
      (lifoo-push it)))

  ;; Pops $it and pushes it on $1
  (define-lisp-word :push (nil)
    (let ((it (lifoo-pop))
          (seq (lifoo-peek)))
      (cond
        ((arrayp seq)
         (vector-push-extend it seq))
        (t
         (push it (lifoo-peek))))))

  ;; Pops $seq and pushes reversed
  (define-lisp-word :reverse (nil)
    (lifoo-push (reverse (lifoo-pop))))

  ;; Pops $fn and $seq,
  ;; and pushes result of mapping $fn over $seq
  (define-macro-word :map (in out)
    (cons (cons in
                `(let ((seq (lifoo-pop)))
                   (lifoo-push
                    (map
                     (cond
                       ((stringp seq) 'string)
                       ((vectorp seq) 'vector)
                       (t 'list))
                     (lambda (it)
                       (lifoo-push it)
                       ,@(lifoo-compile (first (first out)))
                       (lifoo-pop))
                     seq))))
          (rest out)))

  ;; Pops $pred and filters $1 by it
  (define-macro-word :filter (in out)
    (cons (cons in
                `(setf (lifoo-peek)
                       (remove-if (lambda (it)
                                    (lifoo-push it)
                                    ,@(lifoo-compile
                                       (first (first out)))
                                    (lifoo-pop))
                                  (lifoo-peek))))
          (rest out)))

  ;; Pops $fn and replaces $1 with reduction by $fn
  (define-macro-word :reduce (in out)
    (cons
     (cons in
           `(setf (lifoo-peek)
                  (reduce
                   (lambda (x y)
                     (lifoo-push x)
                     (lifoo-push y)
                     ,@(lifoo-compile (first (first out)))
                     (lifoo-pop))
                   (lifoo-peek))))
     (rest out))))

(define-init (:stack)
  ;; Pushes stack on stack
  (define-lisp-word :stack (nil)
    (lifoo-push (lifoo-stack)))
  
  ;; Pops stack
  (define-lisp-word :drop (nil)
    (lifoo-pop))

  ;; Swaps $1 and $2
  (define-lisp-word :swap (nil)
    (lifoo-swap))
  
  ;; Pushes $1 on stack
  (define-lisp-word :dup (nil)
    (lifoo-dup))

  ;; Resets stack
  (define-lisp-word :reset (nil)
    (lifoo-reset))

  ;; Pushes backup of stack to environment
  (define-lisp-word :backup (nil)
    (lifoo-backup))

  ;; Pops and restores backup from environment
  (define-lisp-word :restore (nil)
    (lifoo-restore)))

(define-init (:string)
  ;; Pops $val and pushes string representation
  (define-lisp-word :string (nil)
    (let ((val (lifoo-pop)))
      (lifoo-push (if (listp val)
                      (apply #'string! val)
                      (string! val)))))

  ;; Pops $str and pushes uppercase
  (define-lisp-word :upper (nil)
    (lifoo-push (string-upcase (lifoo-pop))))

  ;; Pops $str and pushes lowercase
  (define-lisp-word :lower (nil)
    (lifoo-push (string-downcase (lifoo-pop))))

  ;; Pops $args and $fmt,
  ;; and pushes formatted output
  (define-lisp-word :format (nil)
    (let ((args (lifoo-pop))
          (fmt (lifoo-pop)))
      (lifoo-push (apply #'format nil fmt args)))))

(define-init (:thread)
  ;; Yields processor and re-schedules thread
  (define-lisp-word :yield (nil)
    (thread-yield))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :sleep (nil :speed 1)
    (sleep (lifoo-pop)))

  ;; Pops $expr;
  ;; evaluates it in new thread/exec,
  ;; and pushes thread
  (define-lisp-word :spawn (nil :speed 1)
    (let* ((num-args (lifoo-pop))
           (expr (lifoo-pop))
           (exec (make-lifoo
                  :stack (clone
                          (if num-args
                              (subseq (stack *lifoo*) 0 num-args)
                              (stack *lifoo*)))
                  :words (clone (words *lifoo*))))
           (thread (make-thread (lambda ()
                                  (lifoo-eval expr :exec exec)))))
      (lifoo-push thread)))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :wait (nil)
    (lifoo-push (join-thread (lifoo-pop))))

  ;; Pops $buf-len and pushes new channel
  (define-lisp-word :chan (nil)
    (lifoo-push (make-chan :max-length (lifoo-pop))))

  ;; Pops $msg and puts on channel in $1
  (define-lisp-word :send (nil)
    (let ((msg (lifoo-pop)))
      (chan-put (lifoo-peek) msg)))

  ;; Gets and pushes message from channel in $1
  (define-lisp-word :recv (nil)
    (let ((msg (chan-get (lifoo-peek))))
      (lifoo-push msg))))

(define-init (:word)
  ;; Pops $id and pushes word
  (define-lisp-word :word (nil)
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push word)))

  ;; Pops $word and pushes T if MACRO?
    (define-lisp-word :macro? (nil)
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push (macro? word))))

  ;; Pops word and pushes source
  (define-lisp-word :source (nil)
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push (source word))))

  ;; Pops $id and $body,
  ;; and defines word
  (define-lisp-word :define (nil)
    (let* ((id (keyword! (lifoo-pop)))
           (body (lifoo-pop))
           (word (make-lifoo-word :id id :source body)))
      (lifoo-define id word)))

  ;; Pops $id and undefines word
  (define-lisp-word :undefine (nil)
    (let ((id (keyword! (lifoo-pop))))
      (lifoo-undefine id))))
