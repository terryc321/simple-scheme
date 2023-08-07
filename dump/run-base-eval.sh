#!/bin/bash

guile -L . -e '(begin (add-to-load-path "/home/terry/simple-scheme/src/") (use-modules (base-eval)) (repl))' $@

