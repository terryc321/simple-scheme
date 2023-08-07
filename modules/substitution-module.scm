


(define (subst-comma ex sym val)
  (define (recur x)
    (cond
     ((not (pair? x)) (if (eq? x sym)
			  val
			  x))     
     (#t (cons (recur (car x))
	       (recur (cdr x))))))
  (recur ex))



(define (subst-splice ex sym val)
  ;; 
  (define (f x)  
    (cond
     ((not (pair? x)) x)
     ((eq? (car x) sym)
      (append val (f (cdr x))))
     (#t (cons (f (car x))
	       (f (cdr x))))))
  ;; 
  (define (g x)  
    (cond
     ((eq? x sym) val)
     (#t (f x))))
  
  ;; just call g
  (g ex))

