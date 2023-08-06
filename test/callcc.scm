

;; not quite how callcc works ...
;; 
(define forever (lambda (n)
		  (write "n=") (write n) (newline)
		  (callcc (lambda (exit)
			    (exit 3)
			    (forever)))))

(forever 0)

((lambda (x) (+ x x)) 5)

