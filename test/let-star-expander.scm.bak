
;; let-star-expander.scm

;; surround the code in a lambda

(define f 123)
(define f2 "happy go lucky")

;; blocked f f2 expr assignments from escaping?
;; see if returning 
(define let-star-expander
  ((lambda (f f2 expr)
     (begin
       
       (define f 0)
       (define f2 0)
       (set! f (lambda (ex) ; strip cond , now only dealing with (X Y1 Y2 Y3 ..) pairs
		 (begin
		   (display "let-argl=") (display (car ex)) (newline)
		   (display "let-body=") (display (cdr (cdr ex))) (newline)
		   (f2 (car (cdr ex))
		       (cdr (cdr ex))
		       ))))

       ;;(X Y1 Y2 Y3 ..)
       ;; caar = X
       ;; cdar = Y1 Y2 Y3
       (set! f2 (lambda (ex the-body)
		  (begin
		    (display "ex=") (display ex) (newline)
		    (if (null? (cdr ex))
			(append (list 'let
				      (list (car ex)))
				the-body)
			(list 'let
			      (list (car ex)) 
			      (f2 (cdr ex) the-body))))))

       ;; target
       ;;
       ;; (let* ((a 1 2 3)(b 4 5 6)(c 7 8 9))
       ;;   (display "yes")
       ;;   (close-files)
       ;;   (jump))
       ;; =>
       ;; (let ((a 1 2 3))
       ;;   (let ((b 4 5 6))
       ;;     (let ((c 7 8 9))
       ;;       ...body...)))
       (define expr '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9))
		       (display "yes")
		       (close-files)
		       (jump)))
       (f expr)
       f)) 0 0 0))

 
(let-star-expander '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9))
		      (display "yes")
		      (close-files)
		      (jump)))

f
f2

(define f 321)
(define f2 "lucky so far!")
(define expr "something random")

"lets see if expander still works now changed f f2 expr"

(let-star-expander '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9))
		      (display "yes")
		      (close-files)
		      (jump)))

f
f2
expr

(let-star-expander '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9))
		      (display "yes")
		      (close-files)
		      (jump)))


