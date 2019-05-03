(defsystem serializable-object.test
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :description "Test system of serializable-object"
  :license "LGPL"
  :depends-on (:serializable-object
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op :after (op c) (eval (read-from-string "(5am:run! :serializable-object)"))))
