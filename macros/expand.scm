
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









