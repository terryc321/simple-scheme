


;; (defmacro setq2 (v1 v2 e)
;;   (list 'progn (list 'setq v1 e) (list 'setq v2 e)))

;; usage
;; suppose z is 8
;; (setq2 x y (+ z 3))
;; sets both x and y to be 8 +3  or 11

;; (let ((x 0)(y 0)(z 8))
;;   (setq2 x y (+ z 3))
;;   (cons x (cons y (cons z (quote ())))))

(defmacro setq2 (v1 v2 e)  (list 'begin	(list 'set! v1 e) (list 'set! v2 e)))

;; setq2 <---  (mlambda ...)
;;




