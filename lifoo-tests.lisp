(defpackage lifoo-tests
  (:use cl cl4l-compare cl4l-test cl4l-utils lifoo))

(in-package lifoo-tests)

(defmacro lifoo-asseq (res &body body)
  "Asserts that evaluating BODY after stack reset pushes value 
   that compares equal to RES"
  `(asseq ,res (do-lifoo () reset ,@body)))

(define-test (:lifoo :meta)
  (with-lifoo ()
    (lifoo-asseq t
      nil nil?)

    (lifoo-asseq :lifoo
      "lifoo" symbol)

    (lifoo-asseq 3
      1 2 "+" word eval)

    (lifoo-asseq '(1 2 +)
      (1 2 +))

    (lifoo-asseq 3
      (1 2 +) eval)

    (lifoo-asseq 43
      42 (lifoo-push (1+ (lifoo-pop))) lisp eval)

    (lifoo-asseq '(:log (:any :message))
      (:any :message) log dump-log first)

    (assert (eq
             :failed
             (handler-case (do-lifoo ()
                             :any-message error)    
               (lifoo-error (e)
                 (assert (eq :any-message (lifoo-error e)))
                 :failed))))

    (assert (eq
             :failed
             (handler-case (do-lifoo ()
                             (1 2 =) assert)    
               (lifoo-error () :failed))))))

(define-test (:lifoo :stack)
  (with-lifoo ()
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

(define-test (:lifoo :flow)
  (with-lifoo ()
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

(define-test (:lifoo :strings)
  (with-lifoo ()
    (lifoo-asseq "123ABC"
      (1 2 3 abc) string)
    
    (lifoo-asseq "1+2=3"
      "~a+~a=~a" (1 2 3) format)))

(define-test (:lifoo :seqs)
  (with-lifoo ()
    (lifoo-asseq 2
      #(1 2 3) 1 nth)

    (lifoo-asseq #(1 4 3)
      #(1 2 3) 1 4 set-nth)

    (lifoo-asseq 2
      (1 2 3) pop drop pop)
    
    (lifoo-asseq '(1 2 3)
      (1) 2 push 3 push reverse)
    
    (lifoo-asseq #(2 4 6)
      #(1 2 3) (2 *) map)))

(define-test (:lifoo :lists)
  (with-lifoo ()
    (lifoo-asseq '(2 . 1)
      1 2 cons)
    
    (lifoo-asseq '(1 . 2)
      (1 . 2))
    
    (lifoo-asseq '(1 2 3)
      1 2 3 list)
    
    (lifoo-asseq 2
      (1 2 3) rest first)))

(define-test (:lifoo :comparisons)
  (with-lifoo ()
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
    (lifoo-asseq 42
      :foo 42 set drop :foo get)
    
    (lifoo-asseq '((:foo . 42))
      :foo 42 set env)
    
    (lifoo-asseq '(nil . 42)
      :foo dup 42 set drop dup rem swap get cons)

    (lifoo-asseq 42
      :foo 42 set (:foo 43 set) eval :foo get)))

(define-test (:lifoo :io)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :threads)
  (with-lifoo ()
    (lifoo-asseq 42
      1 chan 42 chan-put chan-get)
    
    (lifoo-asseq '(:done . 3)
      0 chan (1 2 + chan-put :done) thread swap 
      chan-get swap drop swap 
      join-thread cons)))
