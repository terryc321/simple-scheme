

;; checks n is positive
;; checks ys not null
;; checks ys is pair
;; if n = 1 done
;; otherwise recur...

(define (kelem n ys cont)
  (cond
   ((< n 1) (list "error : elem n ys cont : n < 1 :  called less than 1 " elem n ys cont))
   ((null? ys) (list "error : elem n ys cont @ ys null no more elements" elem n ys cont))
   ((not (pair? ys)) (list "error : elem n ys cont @ not a pair ys" elem n ys cont))
   ((= n 1) (cont (car ys)))
   (#t (kelem (- n 1) (cdr ys) cont))))

(define (krest xs cont)
  (if (pair? xs)
      (cont (cdr xs))
      (list "error : krest xs cont : xs not a pair , cannot take cdr of not pair" xs cont)))
   
(define (kfirst xs cont)
  (elem 1 xs cont))

(define (ksecond xs cont)
  (elem 2 xs cont))

(define (kthird xs cont)
  (elem 3 xs cont))

(define (kfourth xs cont)
  (elem 4 xs cont))

(define (kfifth xs cont)
  (elem 5 xs cont))

(define (ksixth xs cont)
  (elem 6 xs cont))

(define (kseventh xs cont)
  (elem 7 xs cont))

(define (keighth xs cont)
  (elem 8 xs cont))

(define (knineth xs cont)
  (elem 9 xs cont))

(define (ktenth xs cont)
  (elem 10 xs cont))


(define (kcadr xs cont)
  (kcdr xs (lambda (t1)
	     (kcar t1 cont))))

(define (kcddr xs cont)
  (kcdr xs (lambda (t1)
	     (kcdr t1 cont))))


;; tests

(define kid (lambda (x) x))

(display
 (let ((xs '(a b c d e f g h i j k l m n o p q r)))
   (list
    (kelem 100 xs kid)
    (kelem 1 xs kid)
    (kelem 2 xs kid)
    (kelem 3 xs kid)
    (kelem 4 xs kid)
    (kelem 5 xs kid)
    (kelem 6 xs kid)
    (kelem 7 xs kid)
    (kelem 8 xs kid)
    (kelem 9 xs kid)
    (kelem 10 xs kid)
    (kelem -100 xs kid))))
(newline)


  
(display   
(let ((xs '(a b c d e f g h i j k l m n o p q r)))
  (list (equal? (kelem 1 xs kid) (cons 1 'a))
	(equal? (kelem 2 xs kid) (cons 2 'b))
	(equal? (kelem 3 xs kid) (cons 3 'c))
	(equal? (kelem 4 xs kid) (cons 4 'd))
	(equal? (kelem 5 xs kid) (cons 5 'e))
	(equal? (kelem 6 xs kid) (cons 6 'f))
	(equal? (kelem 7 xs kid) (cons 7 'g))
	(equal? (kelem 8 xs kid) (cons 8 'h))
	(equal? (kelem 9 xs kid) (cons 9 'i))
	(equal? (kelem 10 xs kid) (cons 10 'j)))))

(newline)


	
