#lang racket

(module raquet racket
    (provide (except-out (all-from-out racket) lambda)
             (rename-out [lambda function])))

(module score 'raquet
    (map (function (points) (case points
                             [(0) "love"] [(1) "fifteen"]
                             [(2) "thirty"] [(3) "forty"]))
         (list 0 2)))