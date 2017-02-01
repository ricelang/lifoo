(defpackage lifoo-tests
  (:use cl cl4l-test lifoo))

(in-package lifoo-tests)

(define-test (:lifoo :meta)
  (with-lifoo ()
    (lifoo-assert t
      nil nil?)

    (lifoo-assert :lifoo
      "lifoo" symbol)

    (lifoo-assert 3
      1 2 "+" word eval)
    
    (lifoo-assert '(1 2 +)
      (1 2 +))
    
    (lifoo-assert 3
      (1 2 +) eval)

    (lifoo-assert 3
      (1 2 +) compile eval)
    
    (lifoo-assert 42
      42 (lifoo-pop) lisp eval)
    
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
               (lifoo-assert () :failed))))))

(define-test (:lifoo :stack)
  (with-lifoo ()
    (lifoo-assert '(3 2 1)
      1 2 3 stack)

    ;; Make sure that stack is left intact
    (assert (equal '(3 2 1) (lifoo-stack)))
    
    (lifoo-assert 1
      1 dup drop)
    
    (lifoo-assert 2
      1 2 swap drop)))

(define-test (:lifoo :flow)
  (with-lifoo ()
    (lifoo-assert :ok
      :ok (2 1 <) when)
    
    (lifoo-assert :ok
      :ok (1 2 =) unless)
    
    (lifoo-assert 3
      0 (inc dup 3 >) while drop)
    
    (lifoo-assert '(2 1 0)
      list (push) 3 times)))

(define-test (:lifoo :strings)
  (with-lifoo ()
    (lifoo-assert "123ABC"
      (1 2 3 abc) string)
    
    (lifoo-assert "1+2=3"
      "~a+~a=~a" (1 2 3) format)))

(define-test (:lifoo :lists)
  (with-lifoo ()
    (lifoo-assert '(2 . 1)
      1 2 cons)
    
    (lifoo-assert '(1 . 2)
      (1 . 2))
    
    (lifoo-assert '(1 2 3)
      1 2 3 list)
    
    (lifoo-assert 2
      (1 2 3) rest first)
    
    (lifoo-assert 2
      (1 2 3) pop drop pop)
    
    (lifoo-assert '(1 2 3)
      (1) 2 push 3 push reverse)
    
    (lifoo-assert '(2 4 6)
      (1 2 3) (2 *) map)))

(define-test (:lifoo :comparisons)
  (with-lifoo ()
    (lifoo-assert t
      "abc" "abc" eq?)
    
    (lifoo-assert nil
      "abc" "abcd" eq?)
    
    (lifoo-assert t
      "abc" "abcd" neq?)
    
    (lifoo-assert t
      "abc" "def" lt?)
    
    (lifoo-assert nil
      "abc" "def" gt?)))

(define-test (:lifoo :env)
  (with-lifoo ()
    (lifoo-assert 42
      :foo 42 set drop :foo get)
    
    (lifoo-assert '((:bar . 7) (:foo . 42))
      :foo 42 set :bar 7 set env)
    
    (lifoo-assert '(nil . 42)
      :foo dup 42 set drop dup rem swap get cons)))

(define-test (:lifoo :io)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :threads)
  (with-lifoo ()
    (lifoo-assert 42
      1 chan 42 chan-put chan-get)
    
    (lifoo-assert '(:done . 3)
      0 chan (1 2 + chan-put :done) thread swap 
      chan-get swap drop swap 
      join-thread cons)))
