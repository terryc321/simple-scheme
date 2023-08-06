

(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
  (list x y z)) ;; (1 2 3)

(let* ((x 1)(y 2)) (cons x y)) ; (1 . 2)

(let* ((x 1 2 3)(y 4 5 6)) x y (cons x y) y) ; 6

(let* ((x 1 2 3)(y '(1 2 3 4 5))) x y (cons x y) y) ; (1 2 3 4 5)


