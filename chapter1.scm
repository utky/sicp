(define-module chapter1
  (export abs square average sqrt pascal pascal-spec))
(select-module chapter1)

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (square x)
  (* x x))

(define (average x y)
  (/ (x + y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

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
