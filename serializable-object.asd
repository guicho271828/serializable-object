
(defsystem serializable-object
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LGPL"
  :defsystem-depends-on ()
  :depends-on (:trivia :alexandria :iterate)
  :pathname "src"
  :components ((:file "package"))
  :description "Provides a simple class and API for the objects serializable in a FASL file"
  :in-order-to ((test-op (test-op :serializable-object.test)))
  ;; :defsystem-depends-on (:trivial-package-manager)
  ;; :perform
  #+(or)
  (load-op :before (op c)
           (uiop:symbol-call :trivial-package-manager
                             :ensure-program
                             "minisat"
                             :apt "minisat"
                             :dnf "minisat2"
                             :yum "minisat2"
                             :packman ""
                             :yaourt ""
                             :brew "minisat"
                             :choco ""
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :serializable-object)))
           (uiop:symbol-call :trivial-package-manager
                             :ensure-library
                             "libfixposix"
                             :apt "libfixposix0"
                             :dnf ""
                             :yum ""
                             :packman ""
                             :yaourt ""
                             :brew "libfixposix"
                             :choco ""
                             :from-source (format nil "make -C ~a"
                                                  (asdf:system-source-directory :serializable-object)))))
