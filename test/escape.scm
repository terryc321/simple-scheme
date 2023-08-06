

;; free variable assignment has causal connection outside function

(define a 3)

(define f (lambda () (set! a (+ a 1))))


"should see a increase from 3 to 5 ..."

a

(f)

a

(f)

a

(f)

