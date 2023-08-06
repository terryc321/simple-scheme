
(define f 0)
(set! f (lambda (x) (begin (display "x=") (display x) (newline))))

(f "hello world!\n")

(begin 1)
(begin 1 2)
(begin 1 2 3)
(begin 1 2 3 4)
(begin 1 2 3 4 5)

(begin (begin (begin (begin (begin 1 2 3)(begin 4 5 6)(begin 7 8 9)))))





