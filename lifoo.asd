(asdf:defsystem lifoo
  :description "a Forthy, Lispy language fused with Common Lisp"
  :author "Andreas <codr4life@gmail.com>"
  :license "MIT"
  :depends-on (:bordeaux-threads :cl4l)
  :serial t
  :components ((:file "lifoo")
               (:file "init")
               (:file "tests")))
