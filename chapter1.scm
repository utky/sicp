(define-module chapter1
  (export-all))
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

;; 1.2.2 木構造再帰
(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                      kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

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

;; Q 1.14

; (cc 11 5)
; (+ (cc 11 4)
;    (cc (- 11 (first-denomination 5)
;        5))
; (+ (cc 11 4)
;    (cc (- 11 50)
;        5))
; 
; スタックの伸びる量は coin 1 のケースで最長なる。
; n に比例するので O(n)
; 計算量は
; 50 : 1
; 25 : 2
; 10 : 2 + 5 : 1
; 5 : 2
; 1 : 5
; 
; 65^n かな
; 違った。もっと精細な計算が必要なのだった。
; 
; https://codology.net/post/sicp-solution-exercise-1-14/
; 1 kind of coin で growth を観察すると
; T(n, 1) = 2n + 1
; 
; 2 kind of coin [1, 5] だと
; 
; n から 5 を一回ずつ引いていくので
; n - 5, n - 5 * 2, n - 5 * 3 ...
; となる。
; それぞれに coin 1 のパターンの場合の計算量を足す。
; 
; n/5 + 1 + Σ_i=0^n/5 T(n - 5_i, 1)
; 
; (n/5 + 1) は coin 5 を使って n を引いていく部分の計算、
; Σは各パターンの coin 1 を使った木構造の node, leaf 数。
; 
; このように coin 1 を考えて、coin 5 を考える。ここで coin 1 の計算も再利用する。
; 
; この 2 kind of coin では
; 
; T(n, 2) = 1/5(n^2 + 7n) + 1
; となるのでオーダ記法になおすと
; 
; T(n, 2) = Θ(n^2)
; 
; 3 kind of coins でも n から 10 を1回ずつ引く計算と、その下にぶら下がる
; coin 5 のツリーを接ぎ木するようなイメージ。
; 
; T(n, 3) = n/10 + 1 + Σ_i=0^n/10 T(n - 10i, 2)
; 
; となる。
; 後半のΣが 2 kind of coin で求められる計算になる。
; 
; 同様に T(n, 5) を求めると Θ(n5) になる。
;
; ここから学ぶことができるのは、
; 1．観察せよ
; 2．trivial なケースをまず考えよ
; 3．帰的に構成せよ

;; Q 1.15
;; a. (sine 12.15) でpの作用回数
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; 0.1 になるまでに
;; (1/3)をN回かける
;; N=1 4.05
;; N=2 1.016
;; n=3 
;; 2 回かな？
;; -> やべ 1 と 0.1 勘違いしていた。

;; b. (sine a) の評価で sine の生成するプロセスが使うスペースとステップ数の増加の程度は何か？

;; スタックの消費量は 0.1 >= a*(1/3)^n を満たす n に比例する。
;; -> 0.1 >= a/3^n
;;    1/10 * 3^n >= a
;;    3^n/10 >= a
;;    3^n >= 10a
;;    n = log_3 10a
;; 
;; こうかな。
;; 係数 10 はオーダ記法では無視していいので log_3 かな。これがスペース増加量。
;; ステップ数は p の適用回数分
;; 4 : p および cube の定義
;; 1 : 1/3 にする除算
;; なので 5 * (log_3 10a) かな。
;; 
;; log の等式の計算がおかしかった
;; 
;; https://codology.net/post/sicp-solution-exercise-1-15/


;; べき乗

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))
;; 反復処理にしてTCOさせてスペースを節約した場合
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))
;; これでもステップはO(n)なので節約しようと思うとnが偶数の時に逐次平方を用いる、という手がある。
;; nが偶数の時に成り立つ下記の等式を利用すると計算回数を減らせる
;; b^n = (b^2)^n/2
;; (expt 2 (expt 2 (expt 2 ...)))
;; 2の指数回再帰呼び出しが発生する。

;; Q 1.16

(define (expt-iter-sq b counter product)
  (if (= counter 0)
      product
      (if (= 0 (remainder counter 2))
        ;; ab^n -> a((b^2)^n/2) -> (ab^2)(b^n/2)
        (expt-iter-sq b (- (/ counter 2) 1) (* (square b) product))
        ;; ab^n  -> (ab)(b^n-1)
        (expt-iter-sq b (- counter 1) (* b product)))))

(define (expt-iter-sq-ans b counter product)
  (if (= counter 0)
      product
      (if (= 0 (remainder counter 2))
        (expt-iter-sq-ans (square b) (/ counter 2) (* b product))
        (expt-iter-sq-ans b (- counter 1) (* b product)))))

;; http://community.schemewiki.org/?sicp-ex-1.16
;; なんかちょっと違った