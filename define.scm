

(define alist '((x . 3)(y . z)))

(define old-alist alist)

(define f alist)
(define g alist)

(set-cdr! alist (cons (car alist) (cdr alist)))
(set-car! alist `(p . 4))


f
g

old-alist


