(in-package lifoo)

(define-lifoo-init (:abc)
  (define-binary-words ((number number) :speed 1)
                       + - * / = /= < >)

  ;; Pops $val and pushes T if NIL,
  ;; otherwise NIL
  (define-lisp-word :nil? (nil)
    (lifoo-push (null (lifoo-pop))))

  ;; Pushes clone of $1
  (define-lisp-word :clone (nil)
    (lifoo-push (clone (lifoo-peek))))

  ;; Pops $val and pushes its symbolic representation,
  ;; or a newly generated symbol if NIL
  (define-lisp-word :sym (nil)
    (let ((arg (lifoo-pop)))
      (lifoo-push (keyword! (or arg (gensym))))))

  ;; Increases $1
  (define-lisp-word :inc (nil :speed 1)
    (let* ((cell (lifoo-peek-cell))
           (next (if (lifoo-val cell) (1+ (lifoo-val cell)) 1)))
      (when (lifoo-set cell)
        (funcall (lifoo-set cell) next))
      (setf (lifoo-val cell) next)))

  ;; Decreases $1
  (define-lisp-word :dec (nil :speed 1)
    (let* ((cell (lifoo-peek-cell))
           (next (if (lifoo-val cell) (1- (lifoo-val cell)) -1)))
      (when (lifoo-set cell)
        (funcall (lifoo-set cell) next))
      (setf (lifoo-val cell) next)))

  ;; Pops $val and sets value of $1 to $val
  (define-lisp-word :set (nil :speed 1)
    (let* ((val (lifoo-pop))
           (cell (lifoo-peek-cell))
           (set (lifoo-set cell)))
      (unless set
        (error "missing set: ~a" val))
      (funcall set val)
      (setf (lifoo-peek) val)))

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
  (define-lisp-word :time (nil :speed 1)
    (let* ((fn (lifoo-pop))
           (real (get-internal-real-time))
           (cpu (get-internal-run-time)))
      (lifoo-eval fn)
      (lifoo-push (- (get-internal-run-time) cpu))
      (lifoo-push (- (get-internal-real-time) real))))
  
  ;; Pops $fields and $name,
  ;; and defines struct
  (define-lisp-word :struct (nil)
    (let ((name (lifoo-pop))
          (fields (lifoo-pop)))
      (define-lifoo-struct name fields)))
  
  ;; Pops $str and pushes read result
  (define-lisp-word :read ((string))
    (with-input-from-string (in (lifoo-pop))
      (lifoo-push (lifoo-read :in in))))
  
  ;; Pops $expr and pushes string representation
  (define-lisp-word :write (nil)
    (lifoo-push
     (with-output-to-string (out)
       (lifoo-write (lifoo-pop) :out out))))

  ;; Pops $expr and pushes result of evaluating
  (define-lisp-word :eval (nil)
    (lifoo-eval (lifoo-pop)))

  ;; Replaces $expr with function that evaluates it
  (define-macro-word :@ (in out)
    (cons (cons in
                `(lifoo-push (lambda ()
                               (lifoo-optimize)
                               ,@(lifoo-compile
                                  (first (first out))))))
          (rest out))))

(define-lifoo-init (:array)
  ;; Pops $items and pushes new array
  (define-lisp-word :array (nil :speed 1)
    (let* ((items (lifoo-pop))
           (len (length items)))
      (lifoo-push (make-array len
                              :initial-contents items
                              :adjustable t
                              :fill-pointer len)))))

(define-lifoo-init (:compare)
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

(define-lifoo-init (:crypt)
  ;; Pushes new crypt seed
  (define-lisp-word :crypt-seed (nil)
    (lifoo-push (make-iv :aes)))

  ;; Pops $key and $seed,
  ;; and pushes new crypt
  (define-lisp-word :crypt (nil)
    (let ((key (lifoo-pop))
          (seed (lifoo-pop)))
      (lifoo-push (make-crypt key seed))))

  ;; Pops $msg and $crypt,
  ;; and pushes encrypted message
  (define-lisp-word :encrypt ((string nil))
    (let ((msg (lifoo-pop))
          (crypt (lifoo-pop)))
      (lifoo-push (encrypt crypt msg))))

  ;; Pops $msg and $crypt,
  ;; and pushes decrypted message
  (define-lisp-word :decrypt ((string nil))
    (let ((msg (lifoo-pop))
          (crypt (lifoo-pop)))
      (lifoo-push (decrypt crypt msg)))))

(define-lifoo-init (:env)
  ;; Pushes current environment on stack
  (define-lisp-word :env (nil)
    (lifoo-push (copy-list (index-first (lifoo-env)))))

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
    (index-clear (lifoo-env))))

(define-lifoo-init (:error)
  ;; Pops $test and signals error if NIL
  (define-lisp-word :assert (nil)
    (let* ((test (lifoo-peek))
           (ok? (progn
                  (lifoo-eval test)
                  (lifoo-pop))))
      (unless ok?
        (lifoo-error "assert failed: ~a" test))))

  ;; Pops $expected and $actual,
  ;; and signals error they don't compare equal  
  (define-lisp-word :asseq (nil :speed 1)
    (let ((expected (lifoo-pop))
          (actual (lifoo-peek)))
      (lifoo-eval expected)
      (lifoo-eval actual)
      (unless (zerop (compare (lifoo-pop) (lifoo-pop)))
        (lifoo-error "assert failed: ~a /= ~a"
                     actual expected)))))

(define-lifoo-init (:flow)
  ;; Pops $test, $true and $false;
  ;; and pushes $true if $test, otherwise $false
  (define-lisp-word :cond (nil)
    (let ((test (lifoo-pop))
          (true (lifoo-pop))
          (false (lifoo-pop)))
      (lifoo-eval test)
      (if (lifoo-pop)
          (lifoo-eval true)
          (lifoo-eval false))))
  
  ;; Pops $test and $res;
  ;; and pushes $res if $test, otherwise NIL
  (define-lisp-word :when (nil)
    (let ((test (lifoo-pop))
          (res (lifoo-pop)))
      (lifoo-eval test)
      (if (lifoo-pop)
          (lifoo-eval res)
          (lifoo-push nil))))

  ;; Pops $test and $res;
  ;; and pushes $res unless $test, otherwise NIL
  (define-lisp-word :unless (nil)
    (let ((test (lifoo-pop))
          (res (lifoo-pop)))
      (lifoo-eval test)
      (if (lifoo-pop)
          (lifoo-push nil)
          (lifoo-eval res))))

  ;; Pops $reps and $body;
  ;; and repeats $body $reps times,
  ;; pushing indexes before evaluating body
  (define-lisp-word :times (nil :speed 1)
    (let ((reps (lifoo-pop))
          (body (lifoo-pop)))
        (dotimes (i reps)
          (lifoo-push i)
          (lifoo-eval body))))

  ;; Pops $body and loops until $body pushes nil 
  (define-lisp-word :while (nil)
    (let ((body (lifoo-pop)) more?)
      (do-while ((progn
                   (lifoo-eval body)
                   (setf more? (lifoo-pop)))))))
  
  ;; Pops $fn and defers it until environment is closed
  (define-lisp-word :defer (nil)
    (let ((fn (lifoo-pop)))
      (push fn (lifoo-var (defer-key exec)))))

  ;; Pops $value and throws it 
  (define-lisp-word :throw (nil)
    (lifoo-throw (lifoo-pop)))

  ;; Pops $res and $block,
  ;; evaluates $block while guaranteeing that $res runs
  ;; even in the case of errors.
  (define-lisp-word :always (nil)
    (let ((res (lifoo-pop))
          (blk (lifoo-pop)))
      (unwind-protect
           (lifoo-eval blk)
        (lifoo-eval res))))

  ;; Pops $hnd and $block,
  ;; evaluates $block with $hnd registered as handler
  ;; for LIFOO-THROW.
  (define-lisp-word :catch (nil)
    (let ((hnd (lifoo-pop))
          (blk (lifoo-pop)))
      (handler-case
          (lifoo-eval blk)
        (lifoo-throw (c)
          (lifoo-push (value c))
          (lifoo-eval hnd))))))
  
(define-lifoo-init (:hash)
  ;; Pops $src and pushes new hash table
  (define-lisp-word :hash (nil)
    (let ((src (lifoo-pop))
          (res (make-hash-table :test 'equal)))
      (cond
        ((null src))
        ((hash-table-p src)
         (do-hash-table (k v src)
           (setf (gethash k res) v)))
        ((listp src)
         (dolist (it src)
           (setf (gethash (first it) res) (rest it))))
        (t (error "hash not supported: ~a" src)))
      (lifoo-push res)))
  
  ;; Pops $key and $tbl,
  ;; and pushes value at $key in $tbl
  (define-lisp-word :get (nil)
    (let ((key (lifoo-pop))
          (tbl (lifoo-peek)))
      (lifoo-push-expr (gethash key tbl)
                       :del (lambda ()
                              (remhash key tbl)))))

  (define-word :put (nil)
    :tmp var swap set drop get :tmp var set))

(define-lifoo-init (:io)
  ;; Pops $path and pushes open file
  (define-lisp-word :open ((string))
    (let ((stm (open (lifoo-pop))))
      (lifoo-stream stm)
      (lifoo-push stm)))

  ;; Pops $val and prints it
  (define-lisp-word :print (nil)
    (princ (lifoo-pop)))

  ;; Prints line ending
  (define-lisp-word :ln (nil)
    (terpri)))

(define-lifoo-init (:list)
  (define-binary-words (nil) cons)

  ;; Pops $src and pushes new list
  (define-lisp-word :list (nil)
    (let ((src (lifoo-pop)))
      (cond
        ((hash-table-p src)
         (let ((res))
           (do-hash-table (k v src)
             (push (cons k v) res))
           (lifoo-push res)))
        (t (lifoo-push (map 'list #'identity src))))))

  ;; Pushes rest of $1
  (define-lisp-word :rest ((list))
    (let ((lst (lifoo-peek)))
      (lifoo-push (rest lst)
                  :set (lambda (val)
                         (lifoo-pop)
                         (setf (rest lst) val)
                         (lifoo-push lst)))))

  ;; Pushes first element of $1
  (define-lisp-word :first ((list))
    (let ((lst (lifoo-peek)))
      (lifoo-push (first lst)
                  :set (lambda (val)
                         (lifoo-pop)
                         (lifoo-push (rplaca lst val)))))))

(define-lifoo-init (:log)
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

(define-lifoo-init (:meta)
  ;; Pops $protos and initialises protocols
  (define-lisp-word :init ((list))
    (let ((protos (lifoo-pop)))
      (lifoo-init (if (consp protos) protos (list protos)))))

  ;; Pops $code and pushes compiled code
  (define-lisp-word :compile ((list))
    (let ((code (lifoo-pop)))
      (lifoo-push (cons 'progn
                        (lifoo-compile code)))))

  ;; Pops $expr and pushes function that evaluates as Lisp
  (define-lisp-word :link ((list))
    (let ((expr (lifoo-pop)))
      (lifoo-push (eval `(lambda ()
                           (lifoo-optimize)
                           ,expr)))))
  
  ;; Replaces $expr with function that evaluates as Lisp
  (define-macro-word :$ (in out)
    (cons (cons in
                `(lifoo-push (lambda ()
                               (lifoo-optimize)
                               ,(first (first out)))))
          (rest out))))

(define-lifoo-init (:sequence)
  ;; Pops $idx and pushes item from seq
  (define-lisp-word :nth ((fixnum) :speed 1)
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
    (let ((seq (lifoo-peek)))
      (lifoo-push
       (cond
         ((chan? seq)
          (chan-length seq))
         ((hash-table-p seq)
          (hash-table-count seq))
         (t
          (length seq))))))
  
  ;; Pops item from seq in $1 and pushes it
  (define-lisp-word :pop (nil)
    (let* ((seq (lifoo-peek))
           (it (cond
                 ((arrayp seq)
                  (vector-pop seq))
                 (t
                  (pop (lifoo-peek))))))
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
  (define-lisp-word :reverse ((sequence))
    (lifoo-push (reverse (lifoo-pop))))

  ;; Pops $fn and $seq,
  ;; and pushes result of mapping $fn over $seq
  (define-lisp-word :map (nil :speed 1)
    (let ((fn (lifoo-pop))
          (seq (lifoo-pop)))
      (lifoo-push
       (map
        (cond
          ((stringp seq) 'string)
            ((vectorp seq) 'vector)
            (t 'list))
        (lambda (it)
          (lifoo-push it)
          (lifoo-eval fn)
          (lifoo-pop))
        seq))))

  ;; Pops $pred and filters $1 by it
  (define-lisp-word :filter ((nil sequence))
    (let ((pred (lifoo-pop)))
      (setf (lifoo-peek)
            (remove-if (lambda (it)
                         (lifoo-push it)
                         (lifoo-eval pred)
                         (lifoo-pop))
                       (lifoo-peek)))))

  ;; Pops $fn and replaces $1 with reduction by $fn
  (define-lisp-word :reduce ((nil sequence))
    (let ((fn (lifoo-pop)))
      (setf (lifoo-peek)
            (reduce (lambda (x y)
                      (lifoo-push x)
                      (lifoo-push y)
                      (lifoo-eval fn)
                      (lifoo-pop))
                    (lifoo-peek)))))

  ;; Pops $key and $seq,
  ;; and pushes sorted copy
  (define-lisp-word :sort ((nil sequence) :speed 1)
    (let ((key (lifoo-pop))
          (seq (lifoo-pop)))
      (lifoo-push (sort (copy-list seq)
                        (lambda (x y)
                          (< (compare x y) 0))
                        :key (when key
                               (lambda (x)
                                 (lifoo-push x)
                                 (lifoo-eval key)
                                 (lifoo-pop))))))))

(define-lifoo-init (:stack)
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

(define-lifoo-init (:string)
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

(define-lifoo-init (:string :io)
  ;; Pushes new output stream
  (define-lisp-word :stream (nil)
    (let ((stm (make-string-output-stream)))
      (lifoo-stream stm)
      (lifoo-push stm)))

  ;; Pushes new output stream
  (define-lisp-word :stream-string (nil)
    (lifoo-push (get-output-stream-string (lifoo-peek))))

  ;; Pops $string and pushes input stream
  (define-lisp-word :string-stream (nil)
    (let ((stm (make-string-input-stream (lifoo-pop))))
      (lifoo-stream stm)
      (lifoo-push stm)))

  ;; Pops $fn and $out;
  ;; calls $fn for new lines until nil;
  ;; writes to $out separated by newlines,
  ;; and pushes $out again
  (define-lisp-word :dump-lines (nil)
    (let ((fn (lifoo-pop))
          (out (lifoo-pop))
          (line))
      (do-while ((setf line (progn
                              (lifoo-eval fn)
                              (lifoo-pop))))
        (write-line line out))
      (lifoo-push out)))

  ;; Pops $fn and $in,
  ;; and calls $fn for each line
  (define-lisp-word :slurp-lines (nil)
    (let ((fn (lifoo-pop))
          (in (lifoo-pop))
          (line))
      (do-while ((setf line (read-line in nil)))
        (lifoo-push line)
        (lifoo-eval fn)))))

(define-lifoo-init (:thread)
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
                                  (lifoo-eval expr :exec exec)
                                  (lifoo-pop :exec exec)))))
      (lifoo-push thread)))

  ;; Pops $secs and sleeps that many seconds
  (define-lisp-word :wait (nil)
    (lifoo-push (join-thread (lifoo-pop))))

  ;; Pops $buf-len and pushes new channel
  (define-lisp-word :chan ((fixnum))
    (lifoo-push (make-chan :max-length (lifoo-pop))))

  ;; Pops $msg and puts on channel in $1
  (define-lisp-word :send (nil)
    (let ((msg (lifoo-pop)))
      (chan-put (lifoo-peek) msg)))

  ;; Gets and pushes message from channel in $1
  (define-lisp-word :recv (nil)
    (let ((msg (chan-get (lifoo-peek))))
      (lifoo-push msg))))

(define-lifoo-init (:word)
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
           (word (make-lifoo-word
                  :id id
                  :source (if (functionp body)
                              (function-lambda-expression body)
                              body)
                  :fn (when (functionp body)
                        body))))
      (lifoo-define id word)))

  ;; Pops $id and undefines word
  (define-lisp-word :undefine (nil)
    (let ((id (keyword! (lifoo-pop))))
      (lifoo-undefine id))))
