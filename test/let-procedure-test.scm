

;; show numbers 0 to 10 ?
(let loop ((i 0))
  (display i) (display " ")
  (if (< i 10)
      (loop (+ i 1))
      #f))

;; weird factorial ... 
(let fact ((n 1)(r 1))
  (display (- n 1)) (display " => ") (display r) (newline)
  (if (< n 30)
      (fact (+ n 1) (* r n))
      r))


  

;; =>
;; (letrec ((loop (lambda (i)      ; define a recursive
;;                   (display i)   ; procedure whose body
;;                   (if (< i 10)  ; is the loop body
;;                       (loop (+ i 1))))))
;;    (loop 0)) ; start the recursion with 0 as arg i


;; (let->compound? '(let loop ((i 0))
;; 		   (display i)
;; 		   (if (< i 10)
;; 		       (loop (+ i 1)))))

;; (let-procedure->compound? '(let loop ((i 0))
;; 			     (display i)
;; 			     (if (< i 10)
;; 				 (loop (+ i 1)))))

