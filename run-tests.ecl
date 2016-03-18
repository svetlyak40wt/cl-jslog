#!/bin/bash

ecl -load lisp/setup.lisp \
    -eval '(declaim (optimize (debug 3) (safety 3) (speed 0)))' \
    -eval '(push (merge-pathnames "") asdf:*central-registry*)' \
    -eval '(ql:quickload :cl-jslog-tests)' \
    -eval '(cl-jslog-tests:run-tests)' \
    -eval '(quit)'
