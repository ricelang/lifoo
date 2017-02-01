(defpackage lifoo-tests
  (:use cl cl4l-test lifoo))

(in-package lifoo-tests)

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
    (lifoo-asseq '(3 2 1)
      1 2 3 stack)

    ;; Make sure that stack is left intact
    (assert (equal '(3 2 1) (lifoo-stack)))
    
    (lifoo-asseq 1
      1 dup drop)
    
    (lifoo-asseq 2
      1 2 swap drop)))

(define-test (:lifoo :flow)
  (with-lifoo ()
    (lifoo-asseq :ok
      :ok (2 1 <) when)
    
    (lifoo-asseq :ok
      :ok (1 2 =) unless)
    
    (lifoo-asseq 3
      0 (inc dup 3 >) while drop)
    
    (lifoo-asseq '(2 1 0)
      list (push) 3 times)))

(define-test (:lifoo :strings)
  (with-lifoo ()
    (lifoo-asseq "123ABC"
      (1 2 3 abc) string)
    
    (lifoo-asseq "1+2=3"
      "~a+~a=~a" (1 2 3) format)))

(define-test (:lifoo :lists)
  (with-lifoo ()
    (lifoo-asseq '(2 . 1)
      1 2 cons)
    
    (lifoo-asseq '(1 . 2)
      (1 . 2))
    
    (lifoo-asseq '(1 2 3)
      1 2 3 list)
    
    (lifoo-asseq 2
      (1 2 3) rest first)
    
    (lifoo-asseq 2
      (1 2 3) pop drop pop)
    
    (lifoo-asseq '(1 2 3)
      (1) 2 push 3 push reverse)
    
    (lifoo-asseq '(2 4 6)
      (1 2 3) (2 *) map)))

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
      :foo dup 42 set drop dup rem swap get cons)))

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
