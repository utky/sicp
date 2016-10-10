#lang sicp

;; Intuitive definition
(define (exam n)
  (if (< n 3)
    n
    (+
      (exam (- n 1))
      (* 2 (exam (- n 2)))
      (* 3 (exam (- n 3))))))

;; Efficient definition with loop
;; f(n)
;;   | n where n < 3
;;   | f(n - 1) + 2f(n -2) + 3f(n - 3)
;; p: acc where n - 1
;; q: acc where n - 2
;; r: acc where n - 3
;; m: cursor
(define (exam2 n)

  (define (exam-iter p q r m)
    (if (> m n)
      p
      (if (< m 3)
        (exam-iter m p q (+ m 1))
        (exam-iter (+ p (* 2 q) (* 3 r)) p q (+ 1 m)))))
  (exam-iter 0 0 0 0))

(define (exam-spec n)
  (= (exam n) (exam2 n)))
