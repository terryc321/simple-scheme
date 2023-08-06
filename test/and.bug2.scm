
;; need to load cond if and

;; interesting as this didnt seem to work
;; let around defmacro
;; rw4 not defined ---- this doesnt work
;; and is not in scope ... 

;; compiling AND
;; is AND expression a proper list ?
;; if so know how many terms , so no need to test if its the end of a expression
;; 

;; enable tracing everything
;;(evil (set! *tracing-all* #t))
;; setting tracing is really difficult to track down if enabled in a random file
;;(trace-all)


;; force declaration of and2 using define
;; like a much more forced letrec
(define and2 (mlambda args
;; let*
;; (let ((and2 #f))
  (let ((rw4 (lambda (e)
	       (cond
		((null? e) #t)
		(#t `(if ,(car e)
			 (and2 ,@(cdr e))
			 #f))))))
    (rw4 args))))










(and2)

(and2 #t)

(and2 #f)

(and2 #t #f)

(and2 #f #f)

(and2 #t #t)

(and2 #t #t #t #t #t #t)

(and2 #f #t #t #t #t #t)

(and2 #t #f #t #t #t #t)

(and2 #t #t #f #t #t #t)

(and2 #t #t #t #f #t #t)

(and2 #t #t #t #t #f #t)

(and2 #t #t #t #t #t #f)

