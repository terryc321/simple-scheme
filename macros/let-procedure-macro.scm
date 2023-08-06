

;; (let loop ((i 0))
;;   (display i)
;;   (if (< i 10)
;;       (loop (+ i 1))))
;; =>
;; (letrec ((loop (lambda (i)      ; define a recursive
;;                   (display i)   ; procedure whose body
;;                   (if (< i 10)  ; is the loop body
;;                       (loop (+ i 1))))))
;;    (loop 0)) ; start the recursion with 0 as arg i
(define let-procedure->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'let)
	 (pair? (cdr ex))
	 (symbol? (car (cdr ex))))))

(define let-procedure->compound
  (lambda (ex)
    (let ((name (cadr ex))
	  (syms (map car (caddr ex)))
	  (vals (map cadr (caddr ex)))
	  (body (cdddr ex)))
      (cons 'letrec (append (list (list (list name
					      (append (list 'lambda syms) body))))
			    (list (cons name vals)))))))

