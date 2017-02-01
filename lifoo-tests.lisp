(defpackage lifoo-tests
  (:use cl cl4l-test lifoo))

(in-package lifoo-tests)

(define-test (:lifoo :meta)
  (assert (eq t (do-lifoo () nil nil?)))
  (assert (eq :lifoo (do-lifoo ()
                       "lifoo" symbol)))
  (assert (= 3 (do-lifoo ()
                 1 2 "+" word eval)))
  (assert (equal '(1 2 +) (do-lifoo ()
                            (1 2 +))))
  (assert (= 3 (do-lifoo ()
                 (1 2 +) eval)))
  (assert (= 3 (do-lifoo ()
                 (1 2 +) compile eval)))
  (assert (= 42
             (do-lifoo ()
               42 (lifoo-pop) lisp eval)))
  
  (assert (eq
           :ok
           (handler-case (do-lifoo ()
                           :any-message error)    
             (lifoo-error (e)
               (assert (eq :any-message (lifoo-error e)))
               :ok))))

  (assert (eq
           :ok
           (handler-case (do-lifoo ()
                           (1 2 =) assert)    
             (lifoo-assert () :ok)))))

(define-test (:lifoo :stack)
  (with-lifoo ()
    (assert (equal '(3 2 1) (do-lifoo ()
                              1 2 3 stack)))
    (assert (equal '(3 2 1) (lifoo-stack))))
  
  (assert (= 1 (do-lifoo ()
                 1 dup drop)))
  (assert (= 2 (do-lifoo ()
                 1 2 swap drop))))

(define-test (:lifoo :flow)
  (assert (eq :ok (do-lifoo ()
                    :ok (2 1 <) when)))
  (assert (eq :ok (do-lifoo ()
                    :ok (1 2 =) unless)))
  (assert (= 3 (do-lifoo ()
                 0 (inc dup 3 >) while drop)))
  (assert (equal '(2 1 0) (do-lifoo ()
                            list (push) 3 times))))

(define-test (:lifoo :strings)
  (assert (string= "123ABC" (do-lifoo () (1 2 3 abc) string)))
  (assert (string= "1+2=3"
                   (do-lifoo () "~a+~a=~a" (1 2 3) format))))

(define-test (:lifoo :lists)
  (assert (equal '(2 . 1) (do-lifoo ()
                            1 2 cons)))
  (assert (equal '(1 . 2) (do-lifoo ()
                            (1 . 2))))
  (assert (equal '(1 2 3) (do-lifoo ()
                            1 2 3 list)))
  (assert (= 2 (do-lifoo ()
                 (1 2 3) rest first)))
  (assert (= 2 (do-lifoo ()
                 (1 2 3) pop drop pop)))
  (assert (equal '(1 2 3) (do-lifoo ()
                            (1) 2 push 3 push reverse)))
  (assert (equal '(2 4 6) (do-lifoo ()
                            (1 2 3) (2 *) map))))

(define-test (:lifoo :comparisons)
  (assert (do-lifoo ()
            "abc" "abc" eq?))
  (assert (not (do-lifoo ()
                 "abc" "abcd" eq?)))
  (assert (do-lifoo ()
            "abc" "abcd" neq?))
  (assert (do-lifoo ()
            "abc" "def" lt?))
  (assert (not (do-lifoo ()
                 "abc" "def" gt?))))

(define-test (:lifoo :env)
  (assert (= 42 (do-lifoo ()
                  :foo 42 set drop :foo get)))
  (assert (equal '((:bar . 7) (:foo . 42))
                 (do-lifoo ()
                   :foo 42 set :bar 7 set env)))
  (assert (equal '(nil . 42)
                 (do-lifoo ()
                   :foo dup 42 set drop dup rem swap get cons))))

(define-test (:lifoo :io)
  (assert (string= (format nil "hello lifoo!~%")
                   (with-output-to-string (out)
                     (let ((*standard-output* out))
                       (do-lifoo ()
                         "hello lifoo!" print ln))))))

(define-test (:lifoo :threads)
  (assert (= 42 (do-lifoo ()
                  1 chan 42 chan-put chan-get)))
  (assert (equal '(:done . 3)
                 (do-lifoo ()
                   0 chan (1 2 + chan-put :done) thread swap 
                   chan-get swap drop swap 
                   join-thread cons))))
