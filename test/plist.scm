

;; implemented slurping lambda args
;; so list should naturally follow
;; since list is primitive , written plist instead
(define plist (lambda args args))

(plist 1 2 3 4)

(plist 4 3 2 1)

(plist 1 2 (plist 3 4) 5 6)

