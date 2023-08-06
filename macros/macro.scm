
;; test suite
;; unit tests
;; error handling ?


(define cond->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'cond))))

; strip cond , now only dealing with (X Y1 Y2 Y3 ..) pairs
(define cond->compound
  (lambda (ex) (cond-helper (cdr ex))))

(define cond-helper
  (lambda (ex)
    (if (null? ex)
	''()
	(if (eq? (caar ex) 'else)
	    (cons 'begin (cdar ex))
	    (list 'if (caar ex) (append (list 'begin) (cdar ex)) (cond-helper (cdr ex)))))))




;; ;;(X Y1 Y2 Y3 ..)
;; ;; caar = X
;; ;; cdar = Y1 Y2 Y3




;; (cond->compound
;;  '(cond
;;    ((null? xs) 1 2 3)
;;    (#t 4 5 6)
;;    (#f 7 8 9)
;;    (else 10 11 12)))
;; ;; (if (null? xs) (begin 1 2 3) (if #t (begin 4 5 6) (if #f (begin 7 8 9) (begin 10 11 12))))

;; (cond->compound
;;  '(cond
;;    ((null? xs) 1 2 3)
;;    (#t 4 5 6)))

;; (define test
;;   (lambda (xs)
;;     (if (null? xs) (begin 1 2 3) (if #t (begin 4 5 6) (quote ())))))

;; (test '(1 2 3))

;; (define test2
;;   (lambda (xs)
;;     (if (null? xs) (begin 1 2 3) (if #t (begin 4 5 6) (if #f (begin 7 8 9) (begin 10 11 12))))))

;; (test2 '(9 8 7))
 

;; test cases
;;
;; another form of let
;;
;; (let name ... ...body...)
;;

;; (let ((p a b c)...) ...)
(define let->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'let)
	 (pair? (cdr ex))
	 (pair? (car (cdr ex))))))

(define let->compound (lambda (expr)
			(append (list (append (list 'lambda (map car (car (cdr expr))))
					      (list (cons 'begin (cdr (cdr expr))))))
				(map (lambda (x) (cons 'begin (cdr x))) (car (cdr expr))))))

;; (let->compound '(let ((x 1 2 3)(y 4 5 6)(z 7 8 9)) (list x y z)(list y z x)(list z x y)))
;; ;; ((lambda (x y z) (begin (list x y z) (list y z x) (list z x y))) (begin 1 2 3) (begin 4 5 6) (begin 7 8 9))

;; (let->compound '(let ((x 1 2 3)) x))
;; ;; ((lambda (x) (begin x)) (begin 1 2 3))





(define let*->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'let*))))



;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(define let*->compound-helper
  (lambda (ex the-body)
    (begin
      ;; (display "ex=") (display ex) (newline)
      (if (null? (cdr ex))
	  (append (list 'let
			(list (car ex)))
		  the-body)
	  (list 'let
		(list (car ex)) 
		(let*->compound-helper (cdr ex) the-body))))))



(define let*->compound
  (lambda (ex) ; strip cond , now only dealing with (X Y1 Y2 Y3 ..) pairs
    (begin
      ;; (display "let-argl=") (display (car ex)) (newline)
      ;; (display "let-body=") (display (cdr (cdr ex))) (newline)
      (let*->compound-helper (car (cdr ex))
			     (cdr (cdr ex))
			     ))))


;; can see there are quite a few expansions required for let*
;; (expand '(let* ((a 1 2 3)(b 4 5 6)(c 7 8 9))
;; 	   (display "yes")
;; 	   (close-files)
;; 	   (jump)))

;; ;; (let ((a 1 2 3)) (let ((b 4 5 6)) (let ((c 7 8 9)) (display "yes") (close-files) (jump))))
;; (expand '(let ((a 1 2 3)) (let ((b 4 5 6)) (let ((c 7 8 9)) (display "yes") (close-files) (jump)))))
;; (expand '((lambda (a) (begin (let ((b 4 5 6)) (let ((c 7 8 9)) (display "yes") (close-files) (jump))))) (begin 1 2 3)))
;; (expand '((lambda (a) (begin ((lambda (b) (begin (let ((c 7 8 9)) (display "yes") (close-files) (jump)))) (begin 4 5 6)))) (begin 1 2 3)))
;; (expand '((lambda (a) (begin ((lambda (b) (begin ((lambda (c) (begin (display "yes") (close-files) (jump))) (begin 7 8 9)))) (begin 4 5 6)))) (begin 1 2 3)))
;;  ((lambda (a) (begin ((lambda (b) (begin ((lambda (c) (begin (display "yes") (close-files) (jump))) (begin 7 8 9)))) (begin 4 5 6)))) (begin 1 2 3))




(define and->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'and))))


;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(define and->compound-helper
  (lambda (ex)
    (if (null? ex)
	#t
	(if (null? (cdr ex))
	    (car ex)
	    (let ((tmp (gensym "tmp")))
	      (list 'let (list (list tmp (car ex) ))
		    (list 'if tmp tmp (and->compound-helper (cdr ex)))))))))


; strip AND operator , now only dealing with (p1 p2 p3 p4) problems
(define and->compound
  (lambda (ex) 
    (and->compound-helper (cdr ex))))


;; test suite ?
;; and expands into let 

;; (and->compound '(and))
;;  ;; #t
;; (and->compound '(and #t))
;;  ;; #t
;; (and->compound '(and #f))
;;  ;; #f
;; (and->compound '(and #t #t))
;;  ;; (let ((tmp6995 #t)) (if tmp6995 tmp6995 #t))
;; (and->compound '(and #t #f))
;;  ;; (let ((tmp7048 #t)) (if tmp7048 tmp7048 #f))
;; (and->compound '(and #t #t))
;;  ;; (let ((tmp7101 #t)) (if tmp7101 tmp7101 #t))
;; (and->compound '(and #f #f))
;;  ;; (let ((tmp7154 #f)) (if tmp7154 tmp7154 #f))
;; (and->compound '(and #f #f #t))
;;  ;; (let ((tmp7208 #f)) (if tmp7208 tmp7208 (let ((tmp7209 #f)) (if tmp7209 tmp7209 #t))))
;; (and->compound '(and #f #f #f))
;;  ;; (let ((tmp7263 #f)) (if tmp7263 tmp7263 (let ((tmp7264 #f)) (if tmp7264 tmp7264 #f))))
;; (and->compound '(and #f #f #f #t))
;;  ;; (let ((tmp7319 #f)) (if tmp7319 tmp7319 (let ((tmp7320 #f)) (if tmp7320 tmp7320 (let ((tmp7321 #f)) (if tmp7321 tmp73
;; 													 21 #t))))))
;; (and->compound '(and #f #f #f #f))
;;  ;;(let ((tmp7376 #f)) (if tmp7376 tmp7376 (let ((tmp7377 #f)) (if tmp7377 tmp7377 (let ((tmp7378 #f)) (if tmp7378 tmp7378 #f))))))




(define or->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'or))))

;;(X Y1 Y2 Y3 ..)
;; caar = X
;; cdar = Y1 Y2 Y3
(define or->compound-helper
  (lambda (ex)
    (if (null? ex)
	#f
	(if (null? (cdr ex))
	    (car ex)
	    (let ((tmp (gensym "tmp")))
	      (list 'let (list (list tmp (car ex) ))
		    (list 'if tmp tmp (or->compound-helper (cdr ex)))))))))

       
;; strip OR , now only dealing with (p1 p2 p3 p4) problems
(define or->compound
  (lambda (ex) 
    (or->compound-helper (cdr ex))))

       
;; (or->compound '(or))
;;  ;; #f
;; (or->compound '(or #t))
;;  ;; #t
;; (or->compound '(or #f))
;;  ;; #f
;; (or->compound '(or #t #t))
;;  ;; (let ((tmp7824 #t)) (if tmp7824 tmp7824 #t))
;; (or->compound '(or #t #f))
;;  ;; (let ((tmp7877 #t)) (if tmp7877 tmp7877 #f))
;; (or->compound '(or #t #t))
;;  ;; (let ((tmp7930 #t)) (if tmp7930 tmp7930 #t))
;; (or->compound '(or #f #f))
;;  ;; (let ((tmp7983 #f)) (if tmp7983 tmp7983 #f))
;; (or->compound '(or #f #f #t))
;;  ;; (let ((tmp8037 #f)) (if tmp8037 tmp8037 (let ((tmp8038 #f)) (if tmp8038 tmp8038 #t))))
;; (or->compound '(or #f #f #f))
;;  ;; (let ((tmp8092 #f)) (if tmp8092 tmp8092 (let ((tmp8093 #f)) (if tmp8093 tmp8093 #f))))
;; (or->compound '(or #f #f #f #t))
;;  ;; (let ((tmp8148 #f)) (if tmp8148 tmp8148 (let ((tmp8149 #f)) (if tmp8149 tmp8149 (let ((tmp8150 #f)) (if tmp8150 tmp81
;; 													 50 #t))))))
;; (or->compound '(or #f #f #f #f))
;; ;; (let ((tmp8205 #f)) (if tmp8205 tmp8205 (let ((tmp8206 #f)) (if tmp8206 tmp8206 (let ((tmp8207 #f)) (if tmp8207 tmp8207 #f))))))

(define expand-elems
  (lambda (xs)
    (cond
     ((null? xs) '())
     (#t (cons (expand (car xs))
	       (expand-elems (cdr xs)))))))

(define expand
      (lambda (ex)
	(cond
	 ((not (pair? ex)) ex) ;; no macro expand symbols yet
	 ((and (pair? ex) (eq? (car ex) 'quote)) ex) ;; leave quoted alone...?
	 ;; quasi-quote ? unquote ? unquote-splicing ...
	 ((cond->compound? ex) (cond->compound ex))
	 ((let->compound? ex)  (let->compound ex))
	 ((let*->compound? ex) (let*->compound ex))
	 ((and->compound? ex)   (and->compound ex))
	 ((or->compound? ex)    (or->compound ex))
	 (#t (expand-elems ex)))))

(define has-control-word
  (lambda (ex)
    (define hit-word #f)
    (define peek
      (lambda (ex)
	(cond
	 ((null? ex) #f)
	 ((member ex '(and or let let* cond)) (set! hit-word #t))
	 ((pair? ex) (begin
		       (peek (car ex))
		       (peek (cdr ex))))
	 (else #f))))
    (peek ex)
    hit-word))


;; if expanded expression has a control word , more than likely that the expression needs further expansion
(define expand-derived
  (lambda (ex)
    (let ((expand-1 (expand ex)))
      (if (equal? ex expand-1)
	  ex
	  (begin
	    (format #t "expansion => ~a~%" expand-1)
	    (if (has-control-word ex)
		(expand-derived expand-1)
		expand-1))))))









