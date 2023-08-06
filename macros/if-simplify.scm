
;; simplifier removes excess begin statements
;; (if #t x y) => x
(define if->simplify? (lambda (ex) (and (pair? ex)
					  (eq? (car ex) 'if)
					  (pair? (cdr ex))
					  (or (eq? (cadr ex) #t)
					      (eq? (cadr ex) #f)))))


(define if->simplify
  (lambda (ex)
    (cond
     ((eq? (cadr ex) #t) (caddr ex))
     ((eq? (cadr ex) #f) (cadddr ex))
     (#t ex))))


;; (if->simplify? '(if #t 1 2))
;; (if->simplify? '(if #f 1 2))
;; (if->simplify '(if #t 1 2))
;; (if->simplify '(if #f 1 2))

