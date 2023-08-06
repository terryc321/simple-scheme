
;; test suite
;; unit tests
;; error handling ?


(define cond->compound?
  (lambda (ex)
    (and (pair? ex)
	 (eq? (car ex) 'cond))))

; strip cond , now only dealing with (X Y1 Y2 Y3 ..) pairs
(define cond->compound
  (lambda (ex) (cond-helper (cdr ex))))

(define cond-helper
  (lambda (ex)
    (if (null? ex)
	''()
	(if (eq? (caar ex) 'else)
	    (cons 'begin (cdar ex))
	    (list 'if (caar ex) (append (list 'begin) (cdar ex)) (cond-helper (cdr ex)))))))




;; ;;(X Y1 Y2 Y3 ..)
;; ;; caar = X
;; ;; cdar = Y1 Y2 Y3




;; (cond->compound
;;  '(cond
;;    ((null? xs) 1 2 3)
;;    (#t 4 5 6)
;;    (#f 7 8 9)
;;    (else 10 11 12)))
;; ;; (if (null? xs) (begin 1 2 3) (if #t (begin 4 5 6) (if #f (begin 7 8 9) (begin 10 11 12))))

;; (cond->compound
;;  '(cond
;;    ((null? xs) 1 2 3)
;;    (#t 4 5 6)))

;; (define test
;;   (lambda (xs)
;;     (if (null? xs) (begin 1 2 3) (if #t (begin 4 5 6) (quote ())))))

;; (test '(1 2 3))

;; (define test2
;;   (lambda (xs)
;;     (if (null? xs) (begin 1 2 3) (if #t (begin 4 5 6) (if #f (begin 7 8 9) (begin 10 11 12))))))

;; (test2 '(9 8 7))
 
