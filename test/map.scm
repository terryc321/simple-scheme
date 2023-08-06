

(define map2 0)
(define map 0)
(set! map (lambda (f xs)
	    (if
	     (null? xs) xs
	     (cons (f (car xs)) (map f (cdr xs))))))
;; (set! map (lambda (f xs)
;; 	    (cond
;; 	     ((null? xs) xs)
;; 	     (#t (cons (f (car xs)) (map f (cdr xs)))))))


(define twice 0)
(set! twice (lambda (x) (+ x x)))

(map twice (list))
(map twice (list 1))
(map twice (list 1 2))
(map twice (list 1 2 3))
(map twice (list 1 2 3 4))
(map twice (list 1 2 3 4 5))


;;; map f xs

(define (null? xs)
  (eq? xs '()))

;; (define (map f xs)
;;   (if


