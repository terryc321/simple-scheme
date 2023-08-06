


;; internal definitions ......
;; should p exist outside of g ???
(define g (lambda ()
	    (define p 123)
	    (format #t "g.env=~a" $e)
	    p))

(g)

(set! p 456)

(g)

$e






