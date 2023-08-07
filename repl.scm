

;; repl

;;(read)
;;(eval)
;;(print)
;; loop

(define (reader cont)
  (display "ready >")
  (cont (read)))

(define (writer exp cont)
  (display exp)
  (cont #t))

(define (eval exp cont)
  (cont exp))


(define (repl)
  (reader (lambda (in)
	    (eval in (lambda (out)
		       (writer in (lambda (t1)
				    (writer " => " (lambda (t2)
						     (writer out (lambda (t3)
								   (newline)
								   (repl))))))))))))


