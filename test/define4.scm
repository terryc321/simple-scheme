
(define g (lambda (x) (cons x x)))

(g 123) 

;; x should be undefined in environment
;; if DEFINE implementation does not restore original environment after eval'd 2nd part define
;; DEFINE will pollute environment with definition ...

x


$e


