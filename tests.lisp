(defpackage lifoo-tests
  (:use cl cl4l-compare cl4l-test cl4l-utils lifoo))

(in-package lifoo-tests)

(defparameter *reps* 30)

(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY after stack reset pushes value 
   that compares equal to RES"
  `(asseq ,res
     (let* ((compiled 
              (lifoo-compile '(reset ,@body)))
            (fn (eval `(lambda ()
                         ,(lifoo-optimize)
                         ,@compiled))))
       (dotimes (_ *reps*)
         (funcall fn))
       (lifoo-pop))))

(define-fixture (:lifoo)
  (with-lifoo (:env t)
    (lifoo-init t)
    (call-next-fixture)))

(define-test (:lifoo :abc)
  (lifoo-asseq t
    nil nil?)
  
  (lifoo-asseq nil
    1 2 =)
  
  (lifoo-asseq #(1 2 3)
    #(1 2 3) clone pop drop drop)
  
  (lifoo-asseq :lifoo
    "lifoo" sym)

  (do-lifoo ()
    nil sym nil sym neq? assert)

  (lifoo-asseq t
    ((0.0001 sleep) inline 10 times) time 0.0001 <)

  (lifoo-asseq '(1 2 +)
    "1 2 +" read)

  (lifoo-asseq "1 2 +"
    (1 2 +) write)
  
  (lifoo-asseq 3
    (1 2 +) eval)

  (lifoo-asseq 3
    (1 2 +) inline eval))

(define-test (:lifoo :array)
  (lifoo-asseq 2
    #(1 2 3) 1 nth)
  
  (lifoo-asseq #(1 4 3)
    #(1 2 3) 1 nth 4 set drop)

  (lifoo-asseq 3
    #(1 2 3) length)

  (lifoo-asseq #(1 2 3)
    nil array 1 push 2 push 3 push)

  (lifoo-asseq #(2 4 6)
    #(1 2 3) (2 *) inline map)

  (lifoo-asseq 6
    #(1 2 3) (+) reduce))

(define-test (:lifoo :compare)
  (lifoo-asseq t
      "abc" "abc" eq?)
    
  (lifoo-asseq nil
    "abc" "abcd" eq?)
    
  (lifoo-asseq t
    "abc" "abcd" neq?)
    
  (lifoo-asseq t
    "def" "abc" lt?)
    
  (lifoo-asseq nil
    "def" "abc" gt?))

(define-test (:lifoo :crypt)
  (lifoo-asseq "secret message"
    :msg var "secret message" set
    :key var "secret key" set
    :seed var crypt-seed set
    :seed var :key var crypt :msg var encrypt
    :seed var :key var crypt swap decrypt))

(define-test (:lifoo :env)
  (lifoo-asseq 42
    clear
    :foo var 42 set
    :foo var)
    
  (lifoo-asseq '((:bar . 42))
    clear
    :bar var 42 set
    env)
    
  (lifoo-asseq '(nil . 42)
    clear
    :foo var 42 set drop
    :foo var del
    :foo var cons)

  (lifoo-asseq 42
    clear
    :foo var 42 set
    begin :foo var 43 set end
    :foo var))

(define-test (:lifoo :error)
  (assert (eq
           :ok
           (handler-case (do-lifoo ()
                           "message" throw)    
             (lifoo-error () :ok))))

  (assert (eq
           :ok
           (handler-case (do-lifoo ()
                           (1 2 =) assert)    
             (lifoo-error () :ok))))

  (assert (eq
           :ok
           (handler-case (do-lifoo ()
                           1 2 asseq)    
             (lifoo-error () :ok))))

  (lifoo-asseq '(t t t)
    (t t t) (assert) inline map))

(define-test (:lifoo :flow)
  (lifoo-asseq :true
    :false :true (1 1 =) inline cond)
    
  (lifoo-asseq :ok
    :ok (2 1 <) inline when)
    
  (lifoo-asseq :ok
    :ok (1 2 =) unless)
    
  (lifoo-asseq 100
    0 (inc dup 100 >) inline while)
    
  (lifoo-asseq 100
    0 (drop inc) inline 100 times)

  (lifoo-asseq 42
    41
    begin 
      (inc) defer 
      41 asseq
    end)

  (lifoo-asseq :always
    (:frisbee throw
     "skipped" print ln
     (:always) always
     (drop) catch) inline eval)
    
  (lifoo-asseq '(:caught . :frisbee)
    :frisbee throw
    "skipped" print ln
    (:caught cons) catch))

(define-test (:lifoo :io)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :list)
  (lifoo-asseq '(3 . 1)
    1 2 cons first 3 set)
    
  (lifoo-asseq '(1 . 3)
    (1 . 2) rest 3 set)

  (lifoo-asseq 3
    (1 2 3) length)

  (lifoo-asseq 2
    (1 2 3) 1 nth)

  (lifoo-asseq '(1 3)
    (1 2 3) 1 nth del drop)

  (lifoo-asseq 2
    (1 2 3) rest first)

  (lifoo-asseq '(1)
    (1 2 3) rest nil set)

  (lifoo-asseq '(4 2 3)
    (1 2 3) first 4 set)
    
  (lifoo-asseq '(1 2 3)
    nil 1 push 2 push 3 push reverse)

  (lifoo-asseq '(3 7 11)
    (1 2 +) inline
    (3 4 +) inline
    (5 6 +) inline
    stack reverse
    (eval) map))

(define-test (:lifoo :log)
  (lifoo-asseq '((:log (:any :message)))
    (:any :message) log dump-log))

(define-test (:lifoo :meta)
  (with-lifoo ()
    (lifoo-init '(:meta :stack))

    (lifoo-asseq "LIFOO"
      :string init
      "lifoo" upper)

    (lifoo-asseq '(1 . 2)
      (:list) init 2 1 cons))

  (lifoo-asseq 3
    (1 2 +) compile link eval) 
  
  (lifoo-asseq 43
    42
    (lifoo-push (1+ (lifoo-pop)))
    lisp eval)

  (lifoo-asseq 3
    1 2 
    (lifoo:lifoo-push (+ (lifoo:lifoo-pop) (lifoo:lifoo-pop)))
    lisp eval)
  
  (lifoo-asseq 42
    (define-lifoo-init (:foo :bar)
      (define-word :baz () 39 +))
    lisp eval
    (:foo :bar) init
    3 baz))

(define-test (:lifoo :stack)
  (lifoo-asseq '(3 2 1)
    1 2 3 stack)

  (lifoo-asseq 1
    1 dup drop)
    
  (lifoo-asseq 2
    1 2 swap drop)

  (lifoo-asseq '(2 1)
    1 2 backup
    3 4 restore
    stack))

(define-test (:lifoo :string :io)
  (lifoo-asseq (format nil "abc~%def~%ghi~%")
    ("abc" "def" "ghi") stream (pop) dump-lines
    stream-string)
  
  (lifoo-asseq '("abc" "def" "ghi")
    begin
      "abc~%def~%ghi~%" nil format
      string-stream (close) defer
      nil swap (push) slurp-lines
    end reverse))

(define-test (:lifoo :string)
  (lifoo-asseq 3
    "abc" length)

  (lifoo-asseq "bcdbr"
    "abacadabra" (#\a eq?) filter)
    
  (lifoo-asseq "123ABC"
    (1 2 3 abc) string)

  (lifoo-asseq "ac"
    "abc" 1 nth del drop)

  (lifoo-asseq "1+2=3"
    "~a+~a=~a" (1 2 3) format))

(define-test (:lifoo :struct)
  (lifoo-asseq t
    ((bar -1) baz) :foo struct
    nil make-foo foo?)

  (lifoo-asseq '(nil . -1)
    nil make-foo
    foo-bar swap
    foo-baz swap
    drop cons)

  (lifoo-asseq 42
    (:bar 42) make-foo
    foo-bar)

  (lifoo-asseq 43
    (:bar 42) make-foo
    foo-bar 43 set
    foo-bar))

(define-test (:lifoo :thread)
  (lifoo-asseq 42
    1 chan 42 send recv)
    
  (lifoo-asseq '(:done . 3)
    0 chan
    (1 2 + send :done) inline 1 spawn swap 
    recv swap drop swap 
    wait cons))

(define-test (:lifoo :word)
  (lifoo-asseq 3
    1 2 "+" word eval)

  (with-lifoo ()
    (lifoo-init '(:meta :stack :word))
    
    (lifoo-asseq '(+ 1 2)
      (+ 1 2) :foo define
      :foo word source)

    (lifoo-asseq 42
      (drop drop 42) :+ define
      1 2 +)))
