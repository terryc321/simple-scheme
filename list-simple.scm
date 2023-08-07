
(define-module (list-simple))

(use-modules (list-utility))

(format #t "kthree (a b c d) => ~a ~%" (kthree '(a b c d) (lambda (x) x)))


