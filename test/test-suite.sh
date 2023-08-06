#!/bin/bash

# is there a guile way to do this ??

# for any file in test suite directory run it against black
#guile -e '(begin (load "macro.scm")(black))' < ../test/swap.scm > ../out/swap.out

rm -fv /home/terry/lisp/guile/simple-scheme/out/*

# aliases dont work
alias black="guile -e '(begin (load \"macro.scm\")(black))'"

for test in $(ls *.scm);
do
    guile -e '(begin (load \"macro.scm\")(black))' < $test | tee ../out/$test.out
done




