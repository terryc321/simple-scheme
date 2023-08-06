
(define fac 
  (lambda (n)
    (display "n = ") (display n) (newline)
    (if (< n 2) 1 (* n (fac (- n 1))))))

(fac 5)



