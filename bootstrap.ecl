#!/bin/bash

if [[ -e "lisp" ]]; then
    ecl -load lisp/setup.lisp \
        -eval '(push #P"./" asdf:*central-registry*)' \
        -eval '(ql:quickload "cl-jslog-dev")' \
        -eval '(quit)'
else
    curl -O http://beta.quicklisp.org/quicklisp.lisp
    trap 'rm quicklisp.lisp' EXIT

    ecl -load quicklisp.lisp \
        -eval '(quicklisp-quickstart:install :path #P"./lisp")' \
        -eval '(push #P"./" asdf:*central-registry*)' \
        -eval '(ql:quickload "cl-jslog-dev")' \
        -eval '(quit)'
fi
