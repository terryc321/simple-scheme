

;; difficulty is if environment env is passed by value
;; env = '**((a . 1)(b . 2))
;; env = '((c . 3)**(a . 1)(b . 2))
;; ** meaning same memory for ((a . 1)(b . 2))
;; trying to understand mutation in lisp s -expressions

    ;;     * a-1  b-2  c-3
    ;;        ^


    ;; d-4 * a-1 b-2 c-3
    ;;  ^

(define env (vector (list (cons 'a 3)
			  (cons 'b 4)
			  (cons 'c 5))))

env

(define p env)

(define q env)

(vector-ref env 0)

(car (vector-ref env 0))

(set-car! (car (vector-ref env 0))  'z)

env

(set-cdr! (car (vector-ref env 0)) 33)

env

;; extend environment 
;;(set-car! env (cons (cons 'peter 'paul) env)) ;; incorrect

;; (let ((old-env env))
;;   (set-car! env (cons (cons 'peter 'paul) '()))
;;   (set-cdr! env old-env))

;; env

p

q

(define ls (vector-ref env 0))

ls

(vector-set! env 0 (list (cons 'peter 'paul)
			 (cons 'a 3)
			 (cons 'b 4)
			 (cons 'c 5)))

env

p

q


;; extend environment with key value

(define extend (lambda (ev key val)
		 (let ((alist (vector-ref ev 0)))
		   (vector-set! ev 0 (cons (cons key val)
					   alist)))))


env

(extend env 'd 6)

(extend env 'e 7)

(extend env 'f 8)

p

q

env

;; modify in place

;; does it return #t if modified ?

(define modify (lambda (ev key val)
		 (let ((alist (vector-ref ev 0)))
		   (define (recur ys)
		     (cond
		      ((null? ys) #f)
		      ((eq? (car (car ys)) key)
		       (set-cdr! (car ys) val)
		       #t)
		      (#t (recur (cdr ys)))))
		   (if (recur alist)
		       #t
		       #f))))

		   ;; (vector-set! ev 0 (cons (cons key val)
		   ;; 			   alist)))))



(modify env 'a 100)

(modify env 'b 200)

(modify env 'c 300)

(modify env 'd 400)

(modify env 'e 500)

(modify env 'f 600)

(modify env 'g 700)

(modify env 'peter 'saul)

env

p

q


(define modify-or-extend (lambda (ev key val)
			   (let ((r (modify ev key val)))
			     (if r
				 r
				 (extend ev key val)))))


(modify-or-extend env 'a 1100)

(modify-or-extend env 'b 2200)

(modify-or-extend env 'c 3300)

(modify-or-extend env 'd 4400)

(modify-or-extend env 'e 5500)

(modify-or-extend env 'f 6600)

(modify-or-extend env 'g 7700)

(modify-or-extend env 'aa 100)

(modify-or-extend env 'bb 200)

(modify-or-extend env 'cc 300)

(modify-or-extend env 'dd 400)

(modify-or-extend env 'ee 500)

(modify-or-extend env 'ff 600)

(modify-or-extend env 'gg 700)

p

q

env

(define lookup (lambda (ev key)
		 (let ((alist (vector-ref ev 0)))
		   (define (recur ys)
		     (cond
		      ((null? ys) #f)
		      ((eq? (car (car ys)) key)
		       (car ys))
		      (#t (recur (cdr ys)))))
		   (recur alist))))


(lookup env 'gg)

(lookup env 'xx)

(lookup env 'aa)








