

;; or expander

;; pattern matching language
;; (or) #f
;; (or #t) #t
;; (or #f) #f
;; (or #t #f) #t
;; (or #t #t) #t ...

;; in the mit-scheme file --- 
;; (load "../test/genny.scm")


(define f 123)
(define f2 "happy go lucky")

;; blocked f f2 expr assignments from escaping?
;; see if returning 
(define and-expander
  ((lambda (f f2 expr)
     (begin

       ;; (and p1 p2 p3 p4 p5 ...)
       (define f 0)
       (define f2 0)
       (set! f (lambda (ex) ; strip OR , now only dealing with (p1 p2 p3 p4) problems
		 (begin
		   (f2 (cdr ex)))))
       
       ;;(X Y1 Y2 Y3 ..)
       ;; caar = X
       ;; cdar = Y1 Y2 Y3
       (set! f2 (lambda (ex)
		  (if (null? ex)
		      #t
		      (if (null? (cdr ex))
			  (car ex)
			  (let ((tmp (genny)))
			    (list 'let (list (list tmp (car ex) ))
				  (list 'if tmp tmp (f2 (cdr ex)))))))))
       
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
       f)) 0 0 0))


 
;; (and-expander '(and))
;; (and-expander '(and #t))
;; (and-expander '(and #f))
;; (and-expander '(and #t #t))
;; (and-expander '(and #t #f))
;; (and-expander '(and #t #t))
;; (and-expander '(and #f #f))
;; (and-expander '(and #f #f #t))
;; (and-expander '(and #f #f #f))
;; (and-expander '(and #f #f #f #t))
;; (and-expander '(and #f #f #f #f))

f
f2

(define f 321)
(define f2 "lucky so far!")
(define expr "something random")

"lets see if OR expander still works now changed f f2 expr"
 
(and-expander '(and))
(and-expander '(and #t))
(and-expander '(and #f))
(and-expander '(and #t #t))
(and-expander '(and #t #f))
(and-expander '(and #f #f #t))
(and-expander '(and #f #f #f))
(and-expander '(and #f #f #f #t))
(and-expander '(and #f #f #f #f))
;; (and-expander '(and 1 2 3 4 5 6))
;; (and-expander '(and (begin 1 2 3) (begin 4 5 6) (begin 7 8 9) (begin 10 11 12)))

;; (and-expander '(and))

;; (and-expander '(and (begin 1 2 3)))

;; (and-expander '(and (begin 1 2 3) (and (begin 4 5 6) (begin 7 8 9)) (begin 10 11 12)))


(and)
(and #t)
(and #f)
(and #t #t)
(and #t #f)
(and #f #f #t)
(and #f #f #f)
(and #f #f #f #t)
(and #f #f #f #f)



