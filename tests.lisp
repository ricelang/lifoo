(defpackage lifoo-tests
  (:use cl cl4l-compare cl4l-test cl4l-utils lifoo))

(in-package lifoo-tests)

(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY after stack reset pushes value 
   that compares equal to RES"
  `(asseq ,res (do-lifoo () reset ,@body)))

(define-test (:lifoo :abc)
  (with-lifoo ()
    (lifoo-init '(t :flow :sequence :stack :thread))

    (lifoo-asseq t
      nil nil?)

    (lifoo-asseq nil
      1 2 =)

    (lifoo-asseq #(1 2 3)
      #(1 2 3) clone pop drop drop)

    (lifoo-asseq :lifoo
      "lifoo" symbol)

    (lifoo-asseq t
      ((0.0001 sleep) 10 times) time 0.0001 <)
    
    (lifoo-asseq 3
      (1 2 +) eval)))

(define-test (:lifoo :array)
  (with-lifoo ()
    (lifoo-init '(t :array :error :sequence :stack))
    
    (lifoo-asseq 2
      #(1 2 3) 1 nth)

    (lifoo-asseq #(1 4 3)
      #(1 2 3) 1 nth 4 set drop)

    (lifoo-asseq 3
      #(1 2 3) length)

    (lifoo-asseq #(1 2 3)
      nil array 1 push 2 push 3 push)

    (lifoo-asseq 2
      #(1 2 3) pop drop pop)
    
    (lifoo-asseq #(2 4 6)
      #(1 2 3) (2 *) map)

    (lifoo-asseq 6
      #(1 2 3) (+) reduce)))

(define-test (:lifoo :compare)
  (with-lifoo ()
    (lifoo-init '(t :compare :stack))

    (lifoo-asseq t
      "abc" "abc" eq?)
    
    (lifoo-asseq nil
      "abc" "abcd" eq?)
    
    (lifoo-asseq t
      "abc" "abcd" neq?)
    
    (lifoo-asseq t
      "def" "abc" lt?)
    
    (lifoo-asseq nil
      "def" "abc" gt?)))


(define-test (:lifoo :env)
  (with-lifoo ()
    (lifoo-init '(t :env :list :stack))

    (lifoo-asseq 42
      :foo var 42 set :foo var)
    
    (lifoo-asseq '((:bar . 42))
      :bar var 42 set env)
    
    (lifoo-asseq '(nil . 42)
      :foo var 42 set drop :foo var del :foo var cons)

    (lifoo-asseq 42
      :foo var 42 set
      begin :foo var 43 set end
      :foo var)))

(define-test (:lifoo :error)
  (with-lifoo ()
    (lifoo-init '(t :error :flow))
    
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
               (lifoo-error () :ok))))))

(define-test (:lifoo :flow)
  (with-lifoo ()
    (lifoo-init '(t :flow :sequence :stack))
    
    (lifoo-asseq :true
      :false :true (1 1 =) cond)
    
    (lifoo-asseq :ok
      :ok (2 1 <) when)
    
    (lifoo-asseq :ok
      :ok (1 2 =) unless)
    
    (lifoo-asseq 3
      0 (inc dup 3 >) while)
    
    (lifoo-asseq '(2 1 0)
      nil (push) 3 times)

    (lifoo-asseq :always
      :frisbee throw
      "skipped" print ln
      (:always) always
      (drop) catch)
    
    (lifoo-asseq '(:caught . :frisbee)
      :frisbee throw
      "skipped" print ln
      (:caught cons) catch)

    (lifoo-asseq 1
      0 (inc break inc) eval)))

(define-test (:lifoo :io)
  (with-lifoo ()
    (lifoo-init '(t :io))
    
    (assert (string= (format nil "hello lifoo!~%")
                     (with-output-to-string (out)
                       (let ((*standard-output* out))
                         (do-lifoo ()
                           "hello lifoo!" print ln)))))))

(define-test (:lifoo :list)
  (with-lifoo ()
    (lifoo-init '(t :io :list :sequence :stack))

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
      ((1 2 +) (3 4 +) (5 6 +)) (eval) map)))

(define-test (:lifoo :log)
  (with-lifoo ()
    (lifoo-init '(t :log :stack))
    
    (lifoo-asseq '((:log (:any :message)))
      (:any :message) log dump-log)))

(define-test (:lifoo :meta)
  (with-lifoo ()
    (lifoo-init '(t :meta :stack))

    (lifoo-asseq "LIFOO"
      :string init
      "lifoo" upper)

    (lifoo-asseq '(1 . 2)
      (:list) init 2 1 cons)

    (lifoo-asseq 43
      42
      (lifoo-push (1+ (lifoo-pop)))
      lisp eval)))

(define-test (:lifoo :stack)
  (with-lifoo ()
    (lifoo-init '(t :sequence :stack))

    (lifoo-asseq '(1 2 3)
      1 2 3 stack)

    (lifoo-asseq 1
      1 dup drop)
    
    (lifoo-asseq 2
      1 2 swap drop)

    (lifoo-asseq '(1 2)
      1 2 backup
      3 4 restore
      stack)))

(define-test (:lifoo :string)
  (with-lifoo ()
    (lifoo-init '(t :compare :sequence :stack :string))

    (lifoo-asseq 3
      "abc" length)

    (lifoo-asseq "bcdbr"
      "abacadabra" (#\a eq?) filter)
    
    (lifoo-asseq "123ABC"
      (1 2 3 abc) string)

    (lifoo-asseq "ac"
      "abc" 1 nth del drop)

    (lifoo-asseq "1+2=3"
      "~a+~a=~a" (1 2 3) format)))

(define-test (:lifoo :struct)
  (with-lifoo ()
    (lifoo-init '(t :stack :struct))

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
      foo-bar)))

(define-test (:lifoo :thread)
  (with-lifoo ()
    (lifoo-init '(t :list :stack :thread))

    (lifoo-asseq 42
      1 chan 42 send recv)
    
    (lifoo-asseq '(:done . 3)
      0 chan (1 2 + send :done) 1 spawn swap 
      recv swap drop swap 
      wait cons)))

(define-test (:lifoo :word)
  (with-lifoo ()
    (lifoo-init '(t :meta :list :stack :word))
    
    (lifoo-asseq 3
      1 2 "+" word eval)

    (lifoo-asseq '(+ 1 2)
      (+ 1 2) :foo define
      :foo word source)

    (lifoo-asseq 42
      (drop drop 42) :+ define
      1 2 +)))
