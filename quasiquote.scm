


(define (eval-quasiquote exp env cont)
  (define (qq-recur exp)
    (cond
     ((pair? exp)
      (cond
       ((eq? (car exp) 'unquote)
	(base-eval (cadr exp) env (lambda (r) r)))
       ((and (pair? (first exp))     ;; (,@a ?)
	     (eq? (first (first exp)) 'unquote-splicing))
	(base-eval (second (first exp)) ;; eval thing being splice
		   env
		   (lambda (r)
		     (append r (qq-recur (cdr exp))))))
       (#t
	(cons (qq-recur (car exp))
	      (qq-recur (cdr exp)))
	)))
     (#t exp)))
  (cont (qq-recur (cadr exp))))


