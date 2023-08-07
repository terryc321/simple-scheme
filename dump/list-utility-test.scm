

(define-module (list-utility-test)
  #:use-module (srfi srfi-64)
  #:use-module (list-utility))

(test-begin "list-utility-test")

(let ((ys '(a b c d e f g h i j)))
  (test-equal (first ys) 'a)
  (test-equal (second ys) 'b)
  (test-equal (third ys) 'c)
  (test-equal (fourth ys) 'd)
  (test-equal (fifth ys) 'e)
  (test-equal (sixth ys) 'f)
  (test-equal (seventh ys) 'g)
  (test-equal (eighth ys) 'h)
  (test-equal (nineth ys) 'i)
  (test-equal (tenth ys) 'j)
  (test-equal ys (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)))

(let ((ys '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (test-equal (first ys)  1)
  (test-equal (second ys) 2)
  (test-equal (third ys)  3)
  (test-equal (fourth ys) 4)
  (test-equal (fifth ys)  5)
  (test-equal (sixth ys)  6)
  (test-equal (seventh ys) 7)
  (test-equal (eighth ys)  8)
  (test-equal (nineth ys)  9)
  (test-equal (tenth ys)  10)
  (test-equal ys (list 1 2 3 4 5 6 7 8 9 10 11 12)))


(test-end "list-utility-test")



