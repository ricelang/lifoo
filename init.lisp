(in-package lifoo)

(defmacro define-lifoo-init (name &body body)
  "Defines NAME-init around BODY with protocol support"
  (with-symbols (_exec _protos)
    `(defun ,(symbol! 'lifoo- name) (,_protos
                                     &key (,_exec *lifoo*))
       (with-lifoo (:exec ,_exec)
         (macrolet ((with-protos (ps &body pbody)
                      `(when (or (eq t ,',_protos)
                                 (intersection ',ps ,',_protos))
                         ,@pbody)))
           ,@body))
       ,_exec)))

(define-lifoo-init init
  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? ()
    (lifoo-push (null (lifoo-pop))))

  ;; Pushes clone of $1
  (define-lisp-word :clone ()
    (lifoo-push (clone (lifoo-peek))))

  (define-binary-words () + - * / = /= < > cons)

  ;; Increases $1
  (define-lisp-word :inc ()
    (incf (lifoo-peek)))

  ;; Decreases $1
  (define-lisp-word :dec ()
    (decf (lifoo-peek)))
  
  (with-protos (:meta)
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
    (define-lisp-word :eval ()
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
        (lifoo-push (lifoo-undefine id)))))

  (with-protos (:log)
    ;; Pops $word and enables tracing
    (define-lisp-word :trace ()
      (setf (trace? (lifoo-word (lifoo-pop))) t))

    ;; Pops $word and disabled tracing,
    ;; T disables all
    (define-lisp-word :untrace ()
      (let ((expr (lifoo-pop)))
        (if (eq t expr)
            (do-index (_ w (words *lifoo*))
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

  (with-protos (:error)
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
          (lifoo-error "assert failed: ~a /= ~a"
                       actual expected)))))
  
  (with-protos (:stack)
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
  
  (with-protos (:flow)
    ;; Pops $cnd, $true and $false;
    ;; and pushes $true if $cnd, otherwise $false
    (define-lisp-word :cond ()
      (let ((cnd (lifoo-pop))
            (true (lifoo-pop))
            (false (lifoo-pop)))
        (lifoo-eval cnd)
        (lifoo-push (if (lifoo-pop) true false))))
    
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
          (eval (cons 'progn body)))))

    ;; Pops $body and loops until $body pushes nil 
    (define-lisp-word :while ()
      (let ((body (lifoo-parse (lifoo-pop))) (res))
        (do-while ((progn
                     (eval (cons 'progn body))
                     (setf res (lifoo-peek)))
                   res)
          (lifoo-pop)))))
  
  (with-protos (:string)
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
  
  (with-protos (:sequence)
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
  
  (with-protos (:list)
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
  
  (with-protos (:array)
    ;; Pops $items and pushes new array
    (define-lisp-word :array ()
      (let* ((items (lifoo-pop))
             (len (length items)))
        (lifoo-push (make-array len
                                :initial-contents items
                                :adjustable t
                                :fill-pointer len)))))
  
  (with-protos (:compare)
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
  
  (with-protos (:env)
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

    ;; Pops $var;
    ;; deletes it from current environment,
    ;; and pushes value
    (define-lisp-word :del ()
      (let* ((var (lifoo-pop))
             (val (lifoo-get var)))
        (lifoo-del var)
        (lifoo-push val)))

    ;; Opens new environment
    (define-lisp-word :begin ()
      (lifoo-begin))

    ;; Closes current environment
    (define-lisp-word :end ()
      (lifoo-end)))

  (with-protos (:io)
    ;; Pops $val and prints it
    (define-lisp-word :print ()
      (princ (lifoo-pop)))

    ;; Prints line ending
    (define-lisp-word :ln ()
      (terpri)))
  
  (with-protos (:thread)
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
        (lifoo-push msg)))))
