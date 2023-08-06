
(define-module (environment-test)
  #:use-module (srfi srfi-64)
  #:use-module (environment))

(test-begin "environment-test")

;; (let ((env `((a . 1)(b . 2)(c . 3))))
;;   (modify-env! env 'a 4 (lambda (r) r))
;;   (test-equal `((a . 4)(b . 2)(c . 3))
;;     env )
;;   (modify-env! env 'b 3 (lambda (r) r))
;;   (test-equal `((a . 4)(b . 3)(c . 3))
;; 	      env )
;;   (modify-env! env 'c 2 (lambda (r) r))
;;   (test-equal `((a . 4)(b . 3)(c . 2))
;;     env)
;;   (modify-env! env 'd 1 (lambda (r) r))
;;   (test-equal `((d . 1)(a . 4)(b . 3)(c . 2))
;;     env)
;;   )


(let ((env (list (cons 'a 1)
		 (cons 'b 2)
		 (cons 'c 3))))
  (set-env! env 'a 5)
  (test-equal (lookup-env env 'a) (cons 'a 5))
  (test-equal (lookup-env env 'b) (cons 'b 2))
  (test-equal (lookup-env env 'c) (cons 'c 3))
  (extend-env! env 'a 10)
  (test-equal (lookup-env env 'a) (cons 'a 10))
  (test-equal env (list (cons 'a 10)(cons 'a 5)(cons 'b 2)(cons 'c 3)))
  )


(let ((env (list (cons 'a 1))))
  (extend-env! env 'a 5)
  (test-equal (lookup-env env 'a) (cons 'a 5))
  ;; (test-equal (lookup-env env 'b) (cons 'b 2))
  ;; (test-equal (lookup-env env 'c) (cons 'c 3))
  (extend-env! env 'a 10)
  (test-equal (lookup-env env 'a) (cons 'a 10))
  (test-equal env (list (cons 'a 10)(cons 'a 5)))
  )


  ;; (newline)(write "env => ") (write env) (newline)
  ;; (extend-env! env 'a 4 (lambda (r) r))
  ;; (test-equal `((a . 1)(b . 2)(c . 3))
  ;;   env)
  ;; (newline)(write "env => ") (write env) (newline))


;; ;; (test-equal EXPECTED  THE-TESTED-THING)
;; (test-equal '() (make-env) )

;; ;; tag result as a win
;; ;; if it fails for structural reason - malformed environment , not environment or major reason
;; ;; it will not return result (cons 'win anything)
;; ;; if lookup for symbol in environment and symbol is not there
;; ;; that is just a fact its not there , so return on the continue #f false
;; ;; thats not a seg-fault situation
;; ;; just an impact of looking for something that might not be there
;; ;; consequence of reality?
;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win 1) (lookup-env env 'a (lambda (x) x)) )
;;   (test-equal (cons 'win 2) (lookup-env env 'b (lambda (x) x)) )
;;   (test-equal (cons 'win 3) (lookup-env env 'c (lambda (x) x)) )
;;   (test-equal #f (lookup-env env 'd (lambda (x) x))))


;; ;; extend environment
;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal 
;;       (cons 'win '((d . 4)(a . 1)(b . 2)(c . 3)))
;;     (extend-env env 'd 4 (lambda (x) (cons 'win x)))
;;     ))


;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal #f
;;     (modify-env env 'd 4 (lambda (x) x))
;;     ))

;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((a . 10)(b . 2)(c . 3)))
;;     (modify-env env 'a 10 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env))
   

;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((a . 1)(b . 3)(c . 3)))
;;     (modify-env env 'b 3 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env))


;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((a . 1)(b . 2)(c . 12)))
;;     (modify-env env 'c 12 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env ))


;; ;;------------ modify-or-extend ------
;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((a . 111)(b . 2)(c . 3)))
;;     (modify-or-extend-env env 'a 111 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env ))

;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((a . 1)(b . 222)(c . 3)))
;;     (modify-or-extend-env env 'b 222 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env ))

;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((a . 1)(b . 2)(c . 333)))
;;     (modify-or-extend-env env 'c 333 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env ))

;; (let ((env '((a . 1)(b . 2)(c . 3))))
;;   (test-equal (cons 'win '((d . 444)(a . 1)(b . 2)(c . 3)))
;;     (modify-or-extend-env env 'd 444 (lambda (x) x)))
;;   (test-equal '((a . 1)(b . 2)(c . 3))
;;     env ))



    
;; (test-equal (lookup (extend! (make-env) 'a 3) 'a) 3)
;; (test-equal (lookup (extend! (extend! (make-env) 'a 3) 'b 4) 'a) 3)
;; (test-equal (lookup (extend! (extend! (make-env) 'a 3) 'b 4) 'b) 4)
;; (test-equal (extend! (extend! (make-env) 'a 3) 'b 4) '(((b . 4)(a . 3))))
;; (test-equal (extend! (extend! (extend! (make-env) 'a 3)  'b 4) 'c 5) '(((c . 5)(b . 4)(a . 3))))

;; ;; mutating environment
;; (let ((env '(((a . 1)(b . 2)(c . 3)))))
;;   (modify-env! env 'a 10)
;;   (test-equal env '(((a . 10)(b . 2)(c . 3))))
;;   (modify-env! env 'b 3)
;;   (test-equal env '(((a . 10)(b . 3)(c . 3))))
;;   (modify-env! env 'c 4)
;;   (test-equal env '(((a . 10)(b . 3)(c . 4))))
;;   (test-error #t (modify-env! env 'd 15)))

(test-end "environment-test")





