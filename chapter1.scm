(define-module chapter1
  (export-all))
(select-module chapter1)

(use srfi-27)

(define (abs x)
  (if (< x 0)
    (- x)
    x))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

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

;; 正解は
(define (expt-iter-sq-ans b counter product)
  (if (= counter 0)
      product
      (if (= 0 (remainder counter 2))
        (expt-iter-sq-ans (square b) (/ counter 2) (* b product))
        (expt-iter-sq-ans b (- counter 1) (* b product)))))

;; http://community.schemewiki.org/?sicp-ex-1.16
;; なんかちょっと違った

;; Q 1.17
(define (even? x) (= (remainder x 2) 0))
(define (halve x) (/ x 2))
(define (double x) (+ x x))
;; n * m
;; (+ n (+ n (+ n ...)))
;; (+ n (* 2 (+ n (+ n ...))))
(define (mul n m)
  (cond ((= m 0) 0)
        ((even? m) (double (mul n (halve m))))
        (else (+ n (mul n (- m 1))))))
;; Q 1.18
;; 2m * n/2
(define (mul-iter n m)
  (define (go x acc counter)
    (format #t "x ~s acc ~s counter ~s\n" x acc counter)
    (cond ((= counter 0) acc)
          ((even? counter) (go (double x) acc (halve counter)))
          (else (go x (+ acc x) (- counter 1)))))
  (go n 0 m))
;; これは解けた

;; (WIP) Q 1.19 
;; T_pq
;; a <- bq + aq + ap
;; b <- bp + aq
;;
;; こういう形にしておく
;; a <- bq + a(q + p)
;; 
;; a_2 <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;     = bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
;;     = ap^2 + 2bpq + 2apq + 2aq^2 + bq^2
;;     = (2bpq + bq^2) + (ap^2 + 2apq + 2aq^2)
;;     = b(2pq + q^2) + a(p^2 + 2pq + 2q^2)
;; 
;; q' = (2pq + q^2)
;; q' + p' = (p^2 + 2pq + 2q^2)
;; p' = (p^2 + 2pq + 2q^2) - (2pq + q^2)
;; p' = p^2 + q^2

;; b_2 <- (bp + aq) + (bq + aq + ap)q
;; 

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; Q 1.20
;; 最大公約数を求めるユークリッドのアルゴリズム
;; このアルゴリズムは下記の性質を利用している。
;;
;; a % b = r
;; GCD(a, b) = GCD(b, r)
;;
;; これなんでなんだろう。
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; 正規順序評価
;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; あれ、これ引数を先に評価しないと無限に展開されないかな？
;;
;; いや間違ってた再帰的にgcd簡約する前にifを評価する。
;; https://codology.net/post/sicp-solution-exercise-1-20/
;; ifの部分でもremainderの計算が走るので↓は間違い
;; 5 回の簡約で b が 0 になるようだ
;; 5:12
;; 4:6 
;; 3:3
;; 2:1
;; 1:0
;; reminder演算数は3(k-2)で増える
;;
;;（こっちのはあっていそう）
;; 作用的順序だとk回のremainderで済む。
;; この場合は4回


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n)   test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; Q 1.21
;; gosh> (smallest-divisor 199)
;; 199
;; gosh> (smallest-divisor 1999)
;; 1999
;; gosh> (smallest-divisor 19999)
;; 7

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; 自然に考えるとうこう、なのだがなぜ↑の式でもOKなのか調べたい
;; footnote 46 を見るとそのヒントが書いてあるようだ
;; 剰余算の分配測を活用していると言えるようだ
;;; https://ja.wikipedia.org/wiki/%E5%89%B0%E4%BD%99%E6%BC%94%E7%AE%97
(define (expmod-simple base exp m)
  (remainder (expt base exp) m))

;; 3 3 2
;; 3^3 = 27
;; 27 mod 2 = 1
;; 検算
;; (expmod 3 3 2)
;; (remainder (* 3 (expmod 3 2 2)) 2)
;; (remainder (* 3 (remainder (square (expmod 3 1 2)) 2)) 2)
;; (remainder (* 3 (remainder (square (remainder (* 3 (expmod 3 0 2)) 2)) 2)) 2)
;; (remainder (* 3 (remainder (square (remainder (* 3 1) 2)) 2)) 2)
;; (remainder (* 3 (remainder (square (remainder 3 2)) 2)) 2)
;; (remainder (* 3 (remainder (square 1) 2)) 2)
;; (remainder (* 3 (remainder 1 2)) 2)
;; (remainder (* 3 1) 2)
;; (remainder 3 2)
;; 1

;; 3^3 mod 2
;; 3x3x3 mod 2
;; ((3 mod 2) (3 mod 2) (3 mod 2)) mod 2
;; 下記に変換できそうだが一般化できるかわからない
;; (((3 mod 2) * 3 mod 2) * 3 mod 2)
;;
;; 5 * 4 mod 3 = 2
;; ((5 mod 3) * 4 mod 3)
;; (2 * 4 mod 3) = 2

;; (a * b mod n)
;; =
;; ((a mod n) * b) mod n
;; を証明せよ

;; ((a mod n) * b) mod n
;; 分配測から
;; (((a mod n) mod n) * (b mod n)) mod n
;; mod の冪等性から左辺は1回簡約できる
;; ((a mod n) * (b mod n)) mod n
;; 分配測を逆に適用して
;; (a * b mod n)
;; 証明終わり

;; Q 1.22

(define (runtime)
  (current-time))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (time-difference time0 time1)
  (- (time->seconds time0) (time->seconds time1)))

(define (start-prime-test n start-time)
  ;; 上位で prime? の回答を使いたいので束縛しておく
  (let ((is-prime (prime? n)))
    (if is-prime
        (report-prime (time-difference (runtime) start-time)))
    is-prime))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n end found count)
  (if (or (= n end) (= found count))
    #t
    (if (not (= 0 (remainder n 2)))
      (if (timed-prime-test n)
        (search-for-primes (+ n 1) end (+ found 1) count)
        (search-for-primes (+ n 1) end found count))
      (search-for-primes (+ n 1) end found count))))

;; 1009 *** 1.0013580322265625e-5
;; 10007 *** 3.0994415283203125e
;; 100003 *** 9.584426879882812e-5
;; Θ(√n) なので n が 10 倍なら
;; gosh> (sqrt 10)
;; 3.162277665175675
;; かかる。したがって約 3 倍ずつ増加していることが確かに伺える。

(define (smallest-divisor-2 n)
  (find-divisor-2 n 2))

(define (find-divisor-2 n test-divisor)
  (define (next n)
    (cond ((= 2 n) 3)
          (else (+ n 2))))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n)   test-divisor)
        (else (find-divisor-2 n (next test-divisor)))))

(define (prime?-2 n)
  (= n (smallest-divisor-2 n)))

(define (timed-prime-test-2 n)
  (newline)
  (display n)
  (start-prime-test-2 n (runtime)))

(define (start-prime-test-2 n start-time)
  ;; 上位で prime? の回答を使いたいので束縛しておく
  (let ((is-prime (prime?-2 n)))
    (if is-prime
        (report-prime (time-difference (runtime) start-time)))
    is-prime))

(define (search-for-primes-2 n end found count)
  (if (or (= n end) (= found count))
    #t
    (if (not (= 0 (remainder n 2)))
      (if (timed-prime-test-2 n)
        (search-for-primes-2 (+ n 1) end (+ found 1) count)
        (search-for-primes-2 (+ n 1) end found count))
      (search-for-primes-2 (+ n 1) end found count))))

;; Q 1.23
;; gosh> (search-for-primes-2 1000 10000 0 3)
;; 
;; 1009 *** 8.106231689453125e-6
;; 1013 *** 5.0067901611328125e-6
;; 1019 *** 5.0067901611328125e-6#t
;; gosh> (search-for-primes-2 10000 100000 0 3)
;; 
;; 10007 *** 1.71661376953125e-5
;; 10009 *** 1.6927719116210937e-5
;; 10037 *** 1.6927719116210937e-5#t
;; gosh> (search-for-primes-2 100000 1000000 0 3)
;; 
;; 100003 *** 5.698204040527344e-5
;; 100019 *** 5.1021575927734375e-5
;; 100043 *** 5.1975250244140625e-5#t

;; ああ、大体半分かな

(define (timed-prime-test-3 n)
  (newline)
  (display n)
  (start-prime-test-3 n (runtime)))

(define (start-prime-test-3 n start-time)
  ;; 上位で prime? の回答を使いたいので束縛しておく
  (let ((is-prime (fast-prime? n 5)))
    (if is-prime
        (report-prime (time-difference (runtime) start-time)))
    is-prime))

(define (search-for-primes-3 n end found count)
  (if (or (= n end) (= found count))
    #t
    (if (not (= 0 (remainder n 2)))
      (if (timed-prime-test-3 n)
        (search-for-primes-3 (+ n 1) end (+ found 1) count)
        (search-for-primes-3 (+ n 1) end found count))
      (search-for-primes-3 (+ n 1) end found count))))

;; Q 1.24
;; gosh> (search-for-primes-3 1000 10000 0 3)
;; 
;; 1009 *** 3.600120544433594e-5
;; 1013 *** 3.695487976074219e-5
;; 1019 *** 3.910064697265625e-5#t
;; gosh> (search-for-primes-3 10000 100000 0 3)
;; 
;; 10007 *** 4.696846008300781e-5
;; 10009 *** 4.9114227294921875e-5
;; 10037 *** 4.696846008300781e-5#t
;; gosh> (search-for-primes-3 100000 1000000 0 3)
;; 
;; 100003 *** 5.4836273193359375e-5
;; 100019 *** 5.888938903808594e-5
;; 100043 *** 5.602836608886719e-5#t
;; gosh> 

;; 1000 の入力は
;; 1009 *** 3.600120544433594e-5
;; (log 1000) = 3
;; 1000000
;; (log 1000000) = 6
;; なので2倍になっているはず
;; 1000003 *** 6.508827209472656e-5
;; ということで予想通り。

;; Q 1.25

(define (proc-time-elapsed f args)
  (let ((start (runtime))
        (result (apply f args)))
    (display " *** ")
    (display (- (time->seconds (runtime)) (time->seconds start)))
    (newline)
    result))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod-one-remainder base exp m)
  (remainder (fast-expt base exp) m))

;; gosh> (proc-time-elapsed expmod-one-remainder '(2 10000000 2))
;;  *** 18.769355058670044
;; 0
;; gosh> (proc-time-elapsed expmod '(2 10000000 2))
;;  *** 2.7894973754882812e-5
;; 0

;; 圧倒的に違う fast-expt が遅い
;; expmod はスタック積むたびにremainderを取ることでnを小さく保っているのに対して
;; fast-expt は言ったん巨大な数を計算しきってからremainderを呼んでおり、使うメモリのサイズが段違い
;; もしプリミティブ型がコピーだとしたらスタックには巨大数があふれることになる

;; Q 1.26
;; expmod を二回呼び出しているため
;; nが2倍になると
;; n+1で済んでいたのが2(n+1)となってステップ数がΘ(n)になる

;; Q 1.27
;; gosh> (fast-prime? 1105 10)
;; #t
;; gosh> (/ 1105 5)
;; 221

;; Q 1.28
(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (let* ((n (miller-rabin-expmod base (/ exp 2) m))
                (sq-rem (remainder (square n) m)))
            (cond ((and (not (or (= n 1) (= n (- m 1)))) (= sq-rem 1)) 0)
                  (else sq-rem))))
        (else
         (remainder (* base (miller-rabin-expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else #f)))

;; gosh> (fast-prime? 561 5)
;; #t
;; gosh> (miller-rabin-prime? 561 5)
;; #f
;; gosh> (fast-prime? 1729 5)
;; #t
;; gosh> (miller-rabin-prime? 1729 5)
;; #f

;; 1.3 高階手続きによる抽象
(define (inc a) (+ a 1))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a ) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; Q 1.29
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (term k)
    (cond ((= k 0)               (yk k))
          ((= k n)               (yk k))
          ((= (remainder k 2) 0) (* 2 (yk k)))
          (else                  (* 4 (yk k)))))
  (* (/ h 3)
     (sum term 0 inc n)))
