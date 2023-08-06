


(letrec ([is-even? (lambda (n)
		     (or (zero? n)
			 (is-odd? (sub1 n))))]
	 [is-odd? (lambda (n)
		    (and (not (zero? n))
			 (is-even? (sub1 n))))]
	 [sub1 (lambda (n) (- n 1))]
	 [zero? (lambda (n) (= n 0))])
  (is-odd? 11))



