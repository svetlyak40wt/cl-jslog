(asdf:defsystem :cl-jslog
                :description "Command line utility to parse, filter and output JSON logs."
                :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
                                        ;                :depends-on (#:inferior-shell)
                :entry-point "cl-jslog:main"
                :depends-on (#:jonathan #:split-sequence #:asdf)
                :components ((:file "cl-jslog")))
