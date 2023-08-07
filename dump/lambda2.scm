

;; -------- code shows that names to places get over-written by scheme -------

(define fac 10)
(define fac2 200)
(define fac3 300)

(define fac (lambda ()
	      (lambda () (list fac fac2 fac3))))

(define t1 (fac))

"we are here"

(t1)

fac2

fac3

(define fac2  (lambda () (list fac fac2 fac3)))

(fac)

fac2

fac3

(fac2)

"then we get here"

(t1)

;; --------------- internal lambda --------------------
;; internal procedures are 
(define f (lambda ()
	    (begin
	      (define f (lambda () "internal"))
	      "external")))

f
(f) ; external
(f) ; external
(f) ; external

"never get internal returned , define inside a define is not the same as a define on higher level s expression"



