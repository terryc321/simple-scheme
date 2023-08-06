
;; swap macro


;;  startup cmds = {()}
;; (let ((r "r")(s "s"))
;;   (list 'let (list (list 'tmp s))
;; 	(list 'set! s r)
;; 	(list 'set! r 'tmp)))


;; in[1] : (let ((r r) (s s)) (list (quote let) (list (list (quote tmp) s)) (list (quote set!) s r) (list (quote set!) r (quote tmp))))
;; out[1] : (let ((tmp s)) (set! s r) (set! r tmp))

;; introduced tmp variable 
;; (defmacro swap 
;;   (mlambda (r s)	   
;; 	   (let ((tmp s))
;; 	     (set! s r)
;; 	     (set! r tmp))))

;; not got quasi quote available ...
;; list not defined ...

;;(define list (lambda args args))

;; (defmacro swap (r s)
;; 	   (list 'let (list (list 'tmp s))
;; 		 (list 'set! s r)
;; 		 (list 'set! r 'tmp)))

(define swap (mlambda (r s)
		      (list 'let (list (list 'tmp s))
			    (list 'set! s r)
			    (list 'set! r 'tmp))))

	      



;; (let ((r "r")(s "s"))
;;   (list 'let (list (list 'tmp s))
;; 	(list 'set! s r)
;; 	(list 'set! r 'tmp)))

a
b
(swap a b)
a
b
(swap a b)
a
b
'done

;; ;; usage , might always expect pair (3 . 4)
;; (let ((x 3)(y 4))
;;   (swap x y)
;;   (cons x y))

;; should macro expand into
;; (let ((x 3)(y 4))
;;   (let ((tmp y))
;;     (set! y x)
;;     (set! x tmp))
;;   (cons x y))




