
(use-modules (ice-9 pretty-print))
(define pp pretty-print)
;;(define pp display)

(define fourth4 (lambda (xs) (car (cdr (cdr (cdr xs))))))

(define sixth6 (lambda (xs) (car (cdr (cdr (cdr (cdr (cdr xs))))))))

(assert-eq 'd (fourth4 '(a b c d)))

(assert-eq 'f (sixth6 '(a b c d e f)))

(define known-macros '())

(define get-known-macros (lambda () known-macros))

(define install-macro! (lambda (name predicate procedure)
			 (set! known-macros
			       (cons (list (list 'name name)
					   (list 'expand-predicate predicate)
					   (list 'expander procedure))
				     known-macros))))

;; (define collect-matches (lambda (expr)
;; 			  (let ((matches '()))
;; 			    (letrec ((peek (lambda (xs)
;; 					     (cond
;; 					      ((null? xs) matches)
;; 					      (#t (let ((pred (fourth4 (car xs))))
;; 						    (if (pred expr)
;; 							(set! matches (cons (car xs) matches))
;; 							#f))
;; 						  (peek (cdr xs)))))))
;; 			      (peek known-macros))
;; 			    (if (null? (cdr matches))
;; 				(car matches)
;; 				(begin
;; 				  (format #t "macro expander multiple matches for expression~%")
;; 				  (format #t "expr = ~a~%" expr)
;; 				  (format #t "matches = ~A ~%" matches)
;; 				  (error (list 'multiple-matches-on-macro-expansion)))))))

;; (define macro-registered? (lambda (expr)
;; 			    (collect-matches expr)))

(define expand-elems
  (lambda (xs)
    (cond
     ((null? xs) '())
     ((and (pair? xs)   ;; ------- improper lists cause upset??
	   (not (null? (cdr xs)))
	   (not (pair? (cdr xs))))
      (cons (expand (car xs))
	    (cdr xs)))	   
     (#t (cons (expand (car xs))
	       (expand-elems (cdr xs)))))))


(define macro-selector
  (lambda (ex)
    (let ((suitable-expanders
	   (filter (lambda (x) x)
		   (map (lambda (reg-mac)
			  (if ((second (second reg-mac)) ex)
			      reg-mac
			      #f))
			known-macros))))
      (cond
       ((null? suitable-expanders) #f) ;; none suitable
       ((= 1 (length suitable-expanders)) ;; have one and only one expander suitable
	(let ((chosen-expander (first suitable-expanders)))
	  (alist-lookup 'expander chosen-expander)))
       (#t ;; more than one - party time ...
	(format #t "~%WARNING : multiple suitable expanders for expression ~%")
	(let ((chosen-expander (first suitable-expanders)))
	  (alist-lookup 'expander chosen-expander)))))))


;; when i expand , need to update known macros since somebody may have come in and installed a new macro
;; while i was asleep ...
;; if not an s-expression cons pair ... just return it ... maybe later have SYMBOL macros ...
(define expand
  (lambda (ex)
    ;;(let ((preds ...))
    (cond
     ((not (pair? ex)) ex) ;; no macro expand symbols yet
     (#t (let ((expander (macro-selector ex)))
	   (if expander
	       (expander ex)
	       (begin
		 (expand-elems ex))))))))



(define expand-derived
  (lambda (ex)
    (format #t "expansion => ")
    (newline)
    (pp ex)
    (newline)

    (let ((expand-1 (expand ex)))
      (if (equal? ex expand-1)
	  (begin
	    (format #t "final expansion => ")
	    (newline)
	    (pp ex)
	    (newline)	    
	    ex)
	  (expand-derived expand-1)))))


;; (define expand-derived (alist-lookup 'expand-derived macro-expander-module))
;; (define install-macro! (alist-lookup 'install-macro! macro-expander-module ))
;; (define macro? (alist-lookup 'macro-registered? macro-expander-module ))
;; (define known-macros (alist-lookup 'known-macros macro-expander-module ))


;; (assert (procedure? expand-derived))
;; (assert (procedure? install-macro!))
;; (assert (procedure? macro?))
;; (assert (procedure? known-macros))

;; (define macro-expander-module-loaded #t)

