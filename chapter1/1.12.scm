#lang sicp

;; pascal(n, m)
;;   | 0 if n < 1 ∨ m < 1 ∨ n < m
;;   | 1 if m = 1 ∨ m = n
;;   | pascal(n - 1, m - 1) + pascal(n - 1, m)
(define (pascal n m)
  (cond ((or (< n 1) (< m 1) (< n m)) 0)
        ((or (= m 1) (= n m)) 1)
        (else (+ (pascal (- n 1) (- m 1))
                 (pascal (- n 1) m     )))))

(define (pascal-spec)
  (and
    (= (pascal 1 1) 1)
    (= (pascal 1 2) 0)
    (= (pascal 2 1) 1)
    (= (pascal 2 2) 1)
    (= (pascal 3 1) 1)
    (= (pascal 3 2) 2)
    (= (pascal 3 3) 1)
    (= (pascal 4 1) 1)
    (= (pascal 4 2) 3)
    (= (pascal 4 3) 3)
    (= (pascal 4 4) 1)
    ))
