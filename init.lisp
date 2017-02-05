(in-package lifoo)

(define-init (t)
  (define-binary-words () + - * / = /= < > cons)

  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? ()
    (lifoo-push (null (lifoo-pop))))

  ;; Pushes clone of $1
  (define-lisp-word :clone ()
    (lifoo-push (clone (lifoo-peek))))

  ;; Pops $val and pushes its symbolic representation
  (define-lisp-word :symbol ()
    (lifoo-push (keyword! (lifoo-pop))))

  ;; Increases $1
  (define-lisp-word :inc ()
    (incf (lifoo-peek)))

  ;; Decreases $1
  (define-lisp-word :dec ()
    (decf (lifoo-peek)))

  ;; Pops $val and sets value of $1 to $val
  (define-lisp-word :set ()
    (let* ((val (lifoo-pop))
           (cell (lifoo-peek-cell))
           (set (lifoo-set cell)))
      (unless set
        (error "missing set: ~a" val))
      (funcall set val)))

  ;; Pushes and deletes value of $1
  (define-lisp-word :del ()
    (let* ((cell (lifoo-peek-cell))
           (val (lifoo-val cell))
           (del (lifoo-del cell)))
      (unless del
        (error "missing del: ~a" val))
      (funcall del)
      (lifoo-push val)))

  ;; Pops $fields and $name,
  ;; and defines struct
  (define-lisp-word :struct ()
    (let ((name (lifoo-pop))
          (fields (lifoo-pop)))
      (define-lifoo-struct name fields)))
  
  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval ()
    (lifoo-eval (lifoo-pop))))

(define-init (:array)
  ;; Pops $items and pushes new array
  (define-lisp-word :array ()
    (let* ((items (lifoo-pop))
           (len (length items)))
      (lifoo-push (make-array len
                              :initial-contents items
                              :adjustable t
                              :fill-pointer len)))))

(define-init (:compare)
  ;; Pops $rhs and $lhs,
  ;; and pushes result of comparing $lhs to $rhs
  (define-lisp-word :cmp ()
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
  (define-lisp-word :env ()
    (lifoo-push (lifoo-env)))

  ;; Pops $var and returns value
  (define-lisp-word :var ()
    (lifoo-eval (lifoo-pop))
    (let ((var (lifoo-pop)))
      (lifoo-push-expr (lifoo-var var)
                       :del (setf (lifoo-var var) nil))))

  ;; Opens new environment
  (define-lisp-word :begin ()
    (lifoo-begin))

  ;; Closes current environment
  (define-lisp-word :end ()
    (lifoo-end))

  ;; Clears current environment
  (define-lisp-word :clear ()
    (setf (lifoo-env) nil)))

(define-init (:error)
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
        (lifoo-error "assert failed: ~a /= ~a"
                     actual expected)))))

(define-init (:flow)
  ;; Pops $cnd, $true and $false;
  ;; and pushes $true if $cnd, otherwise $false
  (define-lisp-word :cond ()
    (let ((cnd (lifoo-pop))
          (true (lifoo-pop))
          (false (lifoo-pop)))
      (lifoo-eval cnd)
      (lifoo-eval (if (lifoo-pop) true false))))
  
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
  (define-lisp-word :unless ()
    (let ((cnd (lifoo-pop))
          (res (lifoo-pop)))
      (lifoo-eval cnd)
      (if (lifoo-pop)
          (lifoo-push nil)
          (lifoo-eval res))))

  ;; Pops $reps and $body;
  ;; and repeats $body $reps times,
  ;; pushing indexes before evaluating body
  (define-lisp-word :times ()
    (let ((reps (lifoo-pop))
          (body (lifoo-parse (lifoo-pop))))
      (dotimes (i reps)
        (lifoo-push i)
        (eval (cons 'progn body)))))

  ;; Pops $body and loops until $body pushes nil 
  (define-lisp-word :while ()
    (let ((body (lifoo-parse (lifoo-pop))) (res))
      (do-while ((progn
                   (eval (cons 'progn body))
                   (setf res (lifoo-peek)))
                 res)
        (lifoo-pop))
      (lifoo-pop)))

  ;; Pops $value and throws it 
  (define-lisp-word :throw ()
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
  (define-lisp-word :print ()
    (princ (lifoo-pop)))

  ;; Prints line ending
  (define-lisp-word :ln ()
    (terpri)))

(define-init (:list)
  (define-binary-words () cons)

  ;; Pushes stack as list and clears stack
  (define-lisp-word :list ()
    (let ((lst (map 'list #'identity (lifoo-pop))))
      (lifoo-push lst)))

  ;; Pops $list and pushes rest
  (define-lisp-word :rest ()
    (let ((lst (lifoo-pop)))
      (lifoo-push (rest lst)
                  :set (lambda (val)
                         (lifoo-pop)
                         (setf (rest lst) val)
                         (lifoo-push lst)))))

  ;; Pops $list and pushes first element
  (define-lisp-word :first ()
    (let ((lst (lifoo-pop)))
      (lifoo-push (first lst)
                  :set (lambda (val)
                         (lifoo-pop)
                         (lifoo-push (rplaca lst val)))))))

(define-init (:log)
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
    (lifoo-print-log (lifoo-dump-log))))

(define-init (:meta)
  ;; Pops $protos and initialises protocols
  (define-lisp-word :init ()
    (let ((protos (lifoo-pop)))
      (lifoo-init (if (consp protos) protos (list protos)))))
  
  ;; Pops $expr and pushes function that evaluates $expr as Lisp
  (define-lisp-word :lisp ()
    (let ((expr (lifoo-pop)))
      (lifoo-push (make-lifoo-word
                   :source expr
                   :fn (eval `(lambda () ,expr)))))))

(define-init (:sequence)
  ;; Pops $idx and pushes item from seq
  (define-lisp-word :nth ()
    (let* ((idx (lifoo-pop))
           (seq (lifoo-peek))
           (it (cond
                 ((arrayp seq)
                  (aref seq idx))
                 (t
                  (nth idx seq)))))
      (lifoo-push
       it
       :set (lambda (val)
              (cond
                ((arrayp seq)
                 (setf (aref seq idx) val))
                (t
                 (setf (nth idx seq) val))))
       :del (lambda ()
              (lifoo-pop)
              (lifoo-push (remove-nth
                           idx
                           seq))))))
  
  
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
                  (pop seq)))))
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
                     ((vectorp seq) 'vector)
                     (t 'list))
                   (eval `(lambda (it)
                            (lifoo-push it)
                            ,@(lifoo-parse expr)
                            (lifoo-pop)))
                   seq))))

  ;; Pops $pred and filters $1 by it
  (define-lisp-word :filter ()
    (let ((pred (lifoo-parse (lifoo-pop))))
      (setf (lifoo-peek)
            (remove-if (eval `(lambda (it)
                                (lifoo-push it)
                                ,@pred
                                (lifoo-pop)))
                       (lifoo-peek)))))

  ;; Pops $fn and replaces $1 with reduction by $fn
  (define-lisp-word :reduce ()
    (let ((fn (lifoo-parse (lifoo-pop))))
      (setf (lifoo-peek)
            (reduce (eval `(lambda (x y)
                             (lifoo-push x)
                             (lifoo-push y)
                             ,@fn
                             (lifoo-pop)))
                    (lifoo-peek))))))

(define-init (:stack)
  ;; Pushes stack on stack
  (define-lisp-word :stack ()
    (lifoo-push (map 'list #'lifoo-val (stack *lifoo*))))

  ;; Pops stack
  (define-lisp-word :drop ()
    (lifoo-pop))

  ;; Swaps $1 and $2
  (define-lisp-word :swap ()
    (let ((x (lifoo-pop-cell))
          (y (lifoo-pop-cell)))
      (lifoo-push-cell x)
      (lifoo-push-cell y)))  
  
  ;; Pushes $1 on stack
  (define-lisp-word :dup ()
    (lifoo-push (lifoo-peek)))

  ;; Resets stack
  (define-lisp-word :reset ()
    (lifoo-reset))

  (let ((var (gensym)))
    ;; Pushes backup of stack to environment
    (define-lisp-word :backup ()
      (push (copy-seq (stack *lifoo*)) (lifoo-var var)))

    ;; Pops and restores backup from environment
    (define-lisp-word :restore ()
      (let* ((prev (pop (lifoo-var var)))
             (curr (stack *lifoo*))
             (len (length prev)))
        (setf (fill-pointer curr) len)
        (dotimes (i len)
          (setf (aref curr i) (aref prev i)))))))

(define-init (:string)
  ;; Pops $val and pushes string representation
  (define-lisp-word :string ()
    (let ((val (lifoo-pop)))
      (lifoo-push (if (listp val)
                      (apply #'string! val)
                      (string! val)))))

  ;; Pops $str and pushes uppercase
  (define-lisp-word :upper ()
    (lifoo-push (string-upcase (lifoo-pop))))

  ;; Pops $str and pushes lowercase
  (define-lisp-word :lower ()
    (lifoo-push (string-downcase (lifoo-pop))))

  ;; Pops $args and $fmt,
  ;; and pushes formatted output
  (define-lisp-word :format ()
    (let ((args (lifoo-pop))
          (fmt (lifoo-pop)))
      (lifoo-push (apply #'format nil fmt args)))))

(define-init (:thread)
  ;; Yields processor and re-schedules thread
  (define-lisp-word :yield ()
    (thread-yield))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :sleep ()
    (sleep (lifoo-pop)))

  ;; Pops $expr;
  ;; evaluates it in new thread/exec,
  ;; and pushes thread
  (define-lisp-word :spawn ()
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
  (define-lisp-word :wait ()
    (lifoo-push (join-thread (lifoo-pop))))

  ;; Pops $buf-len and pushes new channel
  (define-lisp-word :chan ()
    (lifoo-push (make-chan :max-length (lifoo-pop))))

  ;; Pops $msg and puts on channel in $1
  (define-lisp-word :send ()
    (let ((msg (lifoo-pop)))
      (chan-put (lifoo-peek) msg)))

  ;; Gets and pushes message from channel in $1
  (define-lisp-word :recv ()
    (let ((msg (chan-get (lifoo-peek))))
      (lifoo-push msg))))

(define-init (:word)
  ;; Pops $val and pushes the word it represents
  (define-lisp-word :word ()
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push word)))

  ;; Pops word and pushes source
  (define-lisp-word :source ()
    (let ((word (lifoo-word (lifoo-pop))))
      (lifoo-push (source word))))

  ;; Pops $id and $body,
  ;; and defines word
  (define-lisp-word :define ()
    (let* ((id (keyword! (lifoo-pop)))
           (body (lifoo-pop))
           (word (make-lifoo-word :id id :source body)))
      (lifoo-define id word)))

  ;; Pops $id and undefines word
  (define-lisp-word :undefine ()
    (let ((id (keyword! (lifoo-pop))))
      (lifoo-undefine id))))
