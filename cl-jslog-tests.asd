(asdf:defsystem #:cl-jslog-tests
  :description "Testsuite for cl-jslog"
  :depends-on (#:cl-jslog
               #:should-test
               #:quicklisp-slime-helper)
  :entry-point "cl-jslog-tests:main"
  :components ((:file "cl-jslog-tests")))
