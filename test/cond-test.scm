

(cond (#t 1) (#f 2))
(cond (#f 1) (#t 2))

(cond (#t 1) (#f 2) (#f 3))
(cond (#f 1) (#t 2) (#f 3))
(cond (#f 1) (#f 2) (#t 3))

