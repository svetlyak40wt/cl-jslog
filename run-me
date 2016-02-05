#!/bin/bash

ecl -load lisp/setup.lisp \
    -eval '(push (merge-pathnames "") asdf:*central-registry*)' \
    -eval '(ql:quickload :cl-jslog)' \
    -eval '(cl-jslog:main)'
