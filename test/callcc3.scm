
;; callcc3.scm
;; see if space leak if try looping indefinitely
;;
;; seems there is a space leak if have callcc in tail position.

(define again (lambda (n)
		(format #t "\rn = ~a       " n)
		(callcc (lambda (out)
			  (set! next (lambda ()
				       (again (+ n 1))))))))


(again 1)


;;(loop)
;; (next)
;; (next)


"*************** done ******************"


