(asdf:defsystem #:cl-jslog-dev
                :description "Development environment for cl-jslog"
                :depends-on (#:cl-jslog
                             #:quicklisp-slime-helper
                             #:cl-ppcre))
