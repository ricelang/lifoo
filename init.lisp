(in-package lifoo)

(define-init (:abc)
  (define-binary-words () + - * / = /= < > cons)

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
  (define-lisp-word :inc (nil)
    (incf (lifoo-peek)))

  ;; Decreases $1
  (define-lisp-word :dec (nil)
    (decf (lifoo-peek)))

  ;; Pops $val and sets value of $1 to $val
  (define-lisp-word :set (nil)
    (let* ((val (lifoo-pop))
           (cell (lifoo-peek-cell))
           (set (lifoo-set cell)))
      (unless set
        (error "missing set: ~a" val))
      (funcall set val)))

  ;; Pushes and deletes value of $1
  (define-lisp-word :del (nil)
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
  (define-lisp-word :time ((t))
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
  
  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval (nil)
    (lifoo-eval (lifoo-pop))))

(define-init (:array)
  ;; Pops $items and pushes new array
  (define-lisp-word :array (nil)
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
  (define-word :eq? () cmp 0 =)
  
  (define-word :neq? () cmp 0 /=)
  (define-word :lt? () cmp -1 =)
  (define-word :gt? () cmp 1 =)
  (define-word :lte? () cmp 1 <)
  (define-word :gte? () cmp -1 >))

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
  (define-lisp-word :assert ((t))
    (let* ((cnd (lifoo-pop))
           (ok? (progn (funcall cnd) (lifoo-pop))))
      (unless ok?
        (lifoo-error "assert failed: ~a" cnd))))

  ;; Pops $expected and $actual,
  ;; and signals error they don't compare equal  
  (define-lisp-word :asseq (nil)
    (let ((expected (lifoo-pop))
          (actual (lifoo-pop)))
      (unless (zerop (compare expected actual))
        (lifoo-error "assert failed: ~a /= ~a"
                     actual expected)))))

(define-init (:flow)
  ;; Pops $cnd, $true and $false;
  ;; and pushes $true if $cnd, otherwise $false
  (define-macro-word :cond (in)
    (cons (cons :cond
                `(progn
                   ,@(lifoo-compile (first (first in)))
                   (if (lifoo-pop)
                       (progn ,@(lifoo-compile
                                 (first (second in))))
                       (progn ,@(lifoo-compile
                                 (first (third in)))))))
          (nthcdr 3 in)))
  
  ;; Pops $cnd and $res;
  ;; and pushes $res if $cnd, otherwise NIL
  (define-macro-word :when (in)
    (cons (cons :when
                `(progn
                   ,@(lifoo-compile (first (first in)))
                   (if (lifoo-pop)
                       (progn
                         ,@(lifoo-compile (first (second in))))
                       (lifoo-push nil))))
          (nthcdr 2 in)))

  ;; Pops $cnd and $res;
  ;; and pushes $res unless $cnd, otherwise NIL
  (define-macro-word :unless (in)
    (cons (cons :unless
                `(progn
                   ,@(lifoo-compile (first (first in)))
                   (if (lifoo-pop)
                       (lifoo-push nil)
                       (progn
                         ,@(lifoo-compile (first (second in)))))))
          (nthcdr 2 in)))

  ;; Pops $reps and $body;
  ;; and repeats $body $reps times,
  ;; pushing indexes before evaluating body
  (define-macro-word :times (in)
    (cons
     (cons :times
           `(let ((reps (lifoo-pop)))
              (dotimes (i reps)
                (lifoo-push i)
                ,@(lifoo-compile (first (second in))))))
     (cons (first in) (rest (rest in)))))

  ;; Pops $body and loops until $body pushes nil 
  (define-macro-word :while (in)
    (cons
     (cons :while
           `(progn
              (do-while ((progn
                           ,@(lifoo-compile (first (first in)))
                           (lifoo-peek)))
                (lifoo-pop))
              (lifoo-pop)))
     (rest in)))

  ;; Pops $value and throws it 
  (define-lisp-word :throw (nil)
    (lifoo-throw (lifoo-pop)))

  ;; Wraps parsed forms in unwind-protect with previous
  ;; form as body
  (define-macro-word :always (in)
    (list
     (cons :always `(unwind-protect
                         (progn
                           ,@(reverse (mapcar #'rest (rest in))))
                      (lifoo-eval ',(first (first in)))))))
  
  ;; Wraps parsed forms in handler-case with previous
  ;; form as handler
  (define-macro-word :catch (in)
    (list
     (cons :catch `(handler-case
                       (progn
                         ,@(reverse (mapcar #'rest (rest in))))
                     (lifoo-throw (c)
                       (lifoo-push (value c))
                       (lifoo-eval ',(first (first in))))))))
  
  ;; Breaks out from word
  (define-macro-word :break (in)
    (cons (cons :break `(lifoo-break)) in)))

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
  
  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp (nil)
    (let ((expr (lifoo-pop)))
      (lifoo-push (make-lifoo-word
                   :source expr
                   :fn (eval `(lambda () ,expr)))))))

(define-init (:sequence)
  ;; Pops $idx and pushes item from seq
  (define-lisp-word :nth (nil)
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
  (define-lisp-word :length (nil)
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
  (define-macro-word :map (in)
    (cons (cons :map
                `(let ((seq (lifoo-pop)))
                   (lifoo-push
                    (map
                     (cond
                       ((stringp seq) 'string)
                       ((vectorp seq) 'vector)
                       (t 'list))
                     (lambda (it)
                       (lifoo-push it)
                       ,@(lifoo-compile (first (first in)))
                       (lifoo-pop))
                     seq))))
          (rest in)))

  ;; Pops $pred and filters $1 by it
  (define-macro-word :filter (in)
    (cons (cons :filter
                `(setf (lifoo-peek)
                       (remove-if (lambda (it)
                                    (lifoo-push it)
                                    ,@(lifoo-compile
                                       (first (first in)))
                                    (lifoo-pop))
                                  (lifoo-peek))))
          (rest in)))

  ;; Pops $fn and replaces $1 with reduction by $fn
  (define-macro-word :reduce (in)
    (cons
     (cons :reduce
           `(setf (lifoo-peek)
                  (reduce
                   (lambda (x y)
                     (lifoo-push x)
                     (lifoo-push y)
                     ,@(lifoo-compile (first (first in)))
                     (lifoo-pop))
                   (lifoo-peek))))
     (rest in))))

(define-init (:stack)
  ;; Pushes stack on stack
  (define-lisp-word :stack (nil)
    (lifoo-push (map 'list #'lifoo-val (stack *lifoo*))))

  ;; Pops stack
  (define-lisp-word :drop (nil)
    (lifoo-pop))

  ;; Swaps $1 and $2
  (define-lisp-word :swap (nil)
    (let ((x (lifoo-pop-cell))
          (y (lifoo-pop-cell)))
      (lifoo-push-cell x)
      (lifoo-push-cell y)))  
  
  ;; Pushes $1 on stack
  (define-lisp-word :dup (nil)
    (lifoo-push-cell (lifoo-peek-cell)))

  ;; Resets stack
  (define-lisp-word :reset (nil)
    (lifoo-reset))

  (let ((var (gensym)))
    ;; Pushes backup of stack to environment
    (define-lisp-word :backup (nil)
      (push (copy-seq (stack *lifoo*)) (lifoo-var var)))

    ;; Pops and restores backup from environment
    (define-lisp-word :restore (nil)
      (let* ((prev (pop (lifoo-var var)))
             (curr (stack *lifoo*))
             (len (length prev)))
        (setf (fill-pointer curr) len)
        (dotimes (i len)
          (setf (aref curr i) (aref prev i)))))))

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
  (define-lisp-word :sleep (nil)
    (sleep (lifoo-pop)))

  ;; Pops $expr;
  ;; evaluates it in new thread/exec,
  ;; and pushes thread
  (define-lisp-word :spawn (nil)
    (let* ((num-args (lifoo-pop))
           (expr (lifoo-pop))
           (exec (make-lifoo :stack (clone (if num-args
                                               (subseq
                                                (stack *lifoo*)
                                                0 num-args)
                                               (stack *lifoo*)))
                             :words (clone (words *lifoo*))))
           (thread (make-thread (lambda ()
                                  (lifoo-eval expr
                                              :exec exec)))))
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
  ;; Pops $val and pushes the word it represents
  (define-lisp-word :word (nil)
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push word)))

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
