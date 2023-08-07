

((lambda ()

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

(install-macro! 'let*->compound
		 let*->compound?
		 let*->compound)


))


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


