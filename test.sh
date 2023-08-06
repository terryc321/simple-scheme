#!/bin/bash

cd /home/terry/simple-scheme/src

#guile --no-auto-compile -L . -L ../ environment-test.scm

# test okay
guile --no-auto-compile -L . list-utility-test.scm
guile --no-auto-compile -L . environment-test.scm
guile --no-auto-compile -L . base-eval-test.scm
guile --no-auto-compile -L . closure-test.scm


