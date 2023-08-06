

;; implemented slurping lambda args
;; so list should naturally follow
;; since list is primitive , written plist instead
(define list (lambda args args))

(list 1 2 3 4)

(list 4 3 2 1)

(list 1 2 (list 3 4) 5 6)

