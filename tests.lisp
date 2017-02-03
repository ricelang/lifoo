(defpackage lifoo-tests
  (:use cl cl4l-compare cl4l-test cl4l-utils lifoo))

(in-package lifoo-tests)

(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY after stack reset pushes value 
   that compares equal to RES"
  `(asseq ,res (do-lifoo () reset ,@body)))

(define-test (:lifoo :stack)
  (with-lifoo ()
    (lifoo-init '(:sequence :stack))

    (lifoo-asseq #(1 2 3)
      1 2 3 stack)

    ;; Make sure that stack is left intact
    (assert (zerop (compare #(1 2 3) (lifoo-stack))))

    (lifoo-asseq 42
      stack 42 push)
    
    (lifoo-asseq 1
      1 dup drop)
    
    (lifoo-asseq 2
      1 2 swap drop)

    (lifoo-asseq #(1 2)
      1 2 backup 3 4 restore stack)))

(define-test (:lifoo :basic)
  (with-lifoo ()
    (lifoo-init '(:sequence :stack))

    (lifoo-asseq t
      nil nil?)

    (lifoo-asseq nil
      1 2 =)

    (lifoo-asseq #(1 2 3)
      #(1 2 3) clone pop drop drop)))

(define-test (:lifoo :init)
  (with-lifoo ()
    (lifoo-init '(:init :meta :stack))

    (do-lifoo () :string init)

    (lifoo-asseq "LIFOO"
      "lifoo" upper)

    (do-lifoo () (:list) init)

    (lifoo-asseq '(1 . 2)
      2 1 cons)))

(define-test (:lifoo :meta)
  (with-lifoo ()
    (lifoo-init '(:meta :stack))
    
    (lifoo-asseq :lifoo
      "lifoo" symbol)

    (lifoo-asseq 3
      1 2 "+" word eval)

    (lifoo-asseq 3
      (1 2 +) eval)

    (lifoo-asseq 43
      42 (lifoo-push (1+ (lifoo-pop))) lisp eval)

    (lifoo-asseq 42
      (drop drop 42) :+ define drop
      1 2 :+ word eval)))

(define-test (:lifoo :log)
  (with-lifoo ()
    (lifoo-init '(:log :stack))
    
    (lifoo-asseq '((:log (:any :message)))
      (:any :message) log dump-log)))

(define-test (:lifoo :error)
  (with-lifoo ()
    (lifoo-init '(:error))
    
    (assert (eq
             :ok
             (handler-case (do-lifoo ()
                             "message" error)    
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
    (lifoo-init '(:flow :sequence :stack))
    
    (lifoo-asseq :true
      :false :true (1 1 =) cond)
    
    (lifoo-asseq :ok
      :ok (2 1 <) when)
    
    (lifoo-asseq :ok
      :ok (1 2 =) unless)
    
    (lifoo-asseq 3
      0 (inc dup 3 >) while drop)
    
    (lifoo-asseq '(2 1 0)
      nil (push) 3 times)))

(define-test (:lifoo :string)
  (with-lifoo ()
    (lifoo-init '(:compare :sequence :stack :string))

    (lifoo-asseq 3
      "abc" length)

    (lifoo-asseq "bcdbr"
      "abacadabra" (#\a eq?) filter)
    
    (lifoo-asseq "123ABC"
      (1 2 3 abc) string)
    
    (lifoo-asseq "1+2=3"
      "~a+~a=~a" (1 2 3) format)))

(define-test (:lifoo :list)
  (with-lifoo ()
    (lifoo-init '(:list :sequence :stack))

    (lifoo-asseq '(2 . 1)
      1 2 cons)
    
    (lifoo-asseq '(1 . 2)
      (1 . 2))

    (lifoo-asseq 3
      (1 2 3) length)

    (lifoo-asseq '(1 2 3)
      1 2 3 list)
    
    (lifoo-asseq 2
      (1 2 3) rest first)

    (lifoo-asseq '(1 2 3)
      nil 1 push 2 push 3 push reverse)))

(define-test (:lifoo :array)
  (with-lifoo ()
    (lifoo-init '(:array :error :sequence :stack))
    
    (lifoo-asseq 2
      #(1 2 3) 1 nth)

    (lifoo-asseq #(1 4 3)
      #(1 2 3) 1 4 set-nth)

    (lifoo-asseq 3
      #(1 2 3) length)

    (lifoo-asseq 2
      (1 2 3) array pop drop pop)
    
    (lifoo-asseq #(1 2 3)
      nil array 1 push 2 push 3 push)
    
    (lifoo-asseq #(2 4 6)
      #(1 2 3) (2 *) map)

    (lifoo-asseq 6
      #(1 2 3) (+) reduce
      stack length 2 asseq drop)))

(define-test (:lifoo :compare)
  (with-lifoo ()
    (lifoo-init '(:compare :stack))

    (lifoo-asseq t
      "abc" "abc" eq?)
    
    (lifoo-asseq nil
      "abc" "abcd" eq?)
    
    (lifoo-asseq t
      "abc" "abcd" neq?)
    
    (lifoo-asseq t
      "abc" "def" lt?)
    
    (lifoo-asseq nil
      "abc" "def" gt?)))


(define-test (:lifoo :env)
  (with-lifoo ()
    (lifoo-init '(:env :list :stack))

    (lifoo-asseq 42
      :foo 42 set drop :foo get)
    
    (lifoo-asseq '((:foo . 42))
      :foo 42 set env)
    
    (lifoo-asseq '(nil . 42)
      :foo dup 42 set drop dup del swap get cons)

    (lifoo-asseq 42
      :foo 42 set begin :foo 43 set end :foo get)))

(define-test (:lifoo :io)
  (with-lifoo ()
    (lifoo-init '(:io))
    
    (assert (string= (format nil "hello lifoo!~%")
                     (with-output-to-string (out)
                       (let ((*standard-output* out))
                         (do-lifoo ()
                           "hello lifoo!" print ln)))))))

(define-test (:lifoo :thread)
  (with-lifoo ()
    (lifoo-init '(:list :stack :thread))

    (lifoo-asseq 42
      1 chan 42 chan-put chan-get)
    
    (lifoo-asseq '(:done . 3)
      0 chan (1 2 + chan-put :done) thread swap 
      chan-get swap drop swap 
      join-thread cons)))
