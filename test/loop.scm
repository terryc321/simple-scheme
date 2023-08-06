

;; call itself

(define self (lambda (n)
	       (begin
	       ;;(newline) (display "n = ") (display n)
	       (if (= 0 (mod n 1000)) (begin (format #t "\r n = ~a      " n))
		   #f)
	       ;;(newline)(display "env = ") (display $e) (newline)	       
	       (self (+ n 1)))))

(self 1)



