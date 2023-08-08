


      ;; ((and (pair? ex) (eq? (car ex) 'quote)) ex) ;; leave quoted alone...?
     ;; ;; quasi-quote ? unquote ? unquote-splicing ...
     ;; ((begin->simplify? ex) (begin->simplify ex))
     ;; ((lambda-begin->simplify? ex) (lambda-begin->simplify ex))	 
     ;; ((define->simplify? ex) (define->simplify ex))	 
     ;; ((if->simplify? ex) (if->simplify ex))
     ;; ((cond->compound? ex) (cond->compound ex))
     ;; ((let-procedure->compound? ex)  (let-procedure->compound ex))
     ;; ((let->compound? ex)  (let->compound ex))
     ;; ((let*->compound? ex) (let*->compound ex))
     ;; ((letrec->compound? ex) (letrec->compound ex))
     ;; ((and->compound? ex)   (and->compound ex))
     ;; ((or->compound? ex)    (or->compound ex))
     ;; ((qq->compound? ex)    (qq->compound ex))
     (#t (expand-elems ex)))))


;; macro-selector
;; given an expression select a suitable macro-expander from those currently registered

known-macros

;;(((name while) (expand-predicate #<procedure while->compound? (ex)>) (expander #<procedure while->compound (ex)>)) ((name or) (expand-predicate #<procedure or->compound? (ex)>) (expander #<procedure or->compound (ex)>)) ((name letrec) (expand-predicate #<procedure letrec->compound? (ex)>) (expander #<procedure letrec->compound (ex)>)) ((name let*->compound) (expand-predicate #<procedure let*->compound? (ex)>) (expander #<procedure let*->compound (ex)>)) ((name let-procedure->compound) (expand-predicate #<procedure let-procedure->compound? (ex)>) (expander #<procedure let-procedure->compound (ex)>)) ((name let) (expand-predicate #<procedure let->compound? (ex)>) (expander #<procedure let->compound (expr)>)) ((name lambda-begin-simplify) (expand-predicate #<procedure lambda-begin->simplify? (ex)>) (expander #<procedure lambda-begin->simplify (ex)>)) ((name if->simplify) (expand-predicate #<procedure if->simplify? (ex)>) (expander #<procedure if->simplify (ex)>)) ((name define->simplify) (expand-predicate #<procedure define->simplify? (ex)>) (expander #<procedure define->simplify (ex)>)) ((name cond) (expand-predicate #<procedure cond->compound? (ex)>) (expander #<procedure cond->compound (ex)>)) ((name begin->simplify) (expand-predicate #<procedure begin->simplify? (ex)>) (expander #<procedure cadr (_)>)) ((name and) (expand-predicate #<procedure and->compound? (ex)>) (expander #<procedure and->compound (ex)>)))

;; ex : expression
;; reg-mac : registered macro
;; filter out any non suitable macro expander
;; those that returned false on their respective expand-predicate
;; if only one suitable - extract the expander procedure and done
;; otherwise at an impass - undecided how proceed as more than one expander suitable...
(define macro-selector
  (lambda (ex)
    (let ((suitable-expanders
	   (filter (lambda (x) x)
		   (map (lambda (reg-mac)
			  (if ((second (second reg-mac)) ex)
			      reg-mac
			      #f))
			known-macros))))
      (if (= (length suitable-expanders) 1)
	  (let ((chosen-expander (first suitable-expanders)))
	    (alist-lookup 'expander chosen-expander))
	  (error "macro-selector : multiple matches for expression" (list ex suitable-expanders))))))


(macro-selector '(let ((a 1)) (+ a a)))

(macro-selector '(let loop ((i 0)) (display i) (loop (+ i 1))))



