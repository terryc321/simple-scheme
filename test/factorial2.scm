
(define fac2 (lambda (n r)
	       (format #t "n = ~a : stk = ~a : r = ~a~%~%" n (length $s) r)
	       (if (< n 2)
		   r
		   (fac2 (- n 1) (* r n)))))

(define fac (lambda (n)
	      (format #t "n = ~a : r = ~a~%~%" n (fac2 n 1))))

(define some-number 1000)

(fac some-number)









