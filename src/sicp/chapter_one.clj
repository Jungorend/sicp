(ns sicp.chapter-one
  (:require [clojure.core]))

;; Exercises
;; 1.2
(/ (+ 5 4 (- 2 3 6 (- (/ 4 5))))
   (* 3 (- 6 2) (- 2 7)))

;; 1.3
(defn exercise-1-3
  "This function takes in three arguments, and returns
   the sum of the squares of the two larger numbers."
  [a b c]
  (cond (and (< a b) (< a c)) (+ (* b b) (* c c))
        (and (< b a) (< b c)) (+ (* a a) (* c c))
        :else (+ (* a a) (* b b))))

;; 1.4
(defn a-plus-abs-b
  "This function will add the absolute value of b
   to a. If b is less than 0, it does this by subtracting b."
  [a b]
  ((if (> b 0) + -) a b))

;; 1.5
;; This will return 0 if it is normal order, as y will not be evaluated
;; until needed. If it were evaluated, as in applicative order, the program will call
;; itself repeatedly and no value will be returned, instead it will hang.

;; 1.6
;; the else-clause is an argument and so it will be evaluated when passed to new-if
;; this will lead to sqrt-iter recursively calling itself and the program locking

;; 1.7
;; The issue is that good-enough? is not a factor of the precision of the object.
;; so the square root of a small number will be dealing in the decimals much quicker
;; than a normal one.
(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess prior-guess]
  (if (< prior-guess guess)
    (< 0.9 (/ prior-guess guess))
    (< 0.9 (/ guess prior-guess))))

(defn sqrt-iter [guess x prior-guess]
  (if (good-enough? guess prior-guess)
    guess
    (sqrt-iter (improve guess x)
               x guess)))

(defn sqrt [x]
  (sqrt-iter 1 x 1000))

;; 1.8
;; This just needs an adjusted improve function
(defn improve-cuberoot [guess x]
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(defn cube-iter [guess x prior-guess]
  (if (good-enough? guess prior-guess)
    guess
    (cube-iter (improve-cuberoot guess x)
               x guess)))

(defn cbrt [x]
  (cube-iter 1 x 1000))

;; 1.9
;; the first one is recursive as it needs to recur itself before returning
;; the result to the inc procedure. The second one is iterative as no stack
;; needs to be maintained.

;; 1.10
;; f = 2y
;; g = 2y^y

;; 1.11
(defn f-iter
  "f(n) = n if n < 3
   f(n) = f(n - 1) + 2f(n-2) + 3f(n-3) if n >= 3"
  ([n] (f-iter n 2 2 1 0))
  ([n count current minus-one minus-two]
   (cond (< n 3) n
         (= n count) current
         :else (recur n (inc count) (+ current (* 2 minus-one) (* 3 minus-two)) current minus-one))))

(defn f-recurs
  "f(n) = n if n < 3
   f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3"
  [n]
  (if (< n 3)
    n
    (+ (f-recurs (- n 1)) (* 2 (f-recurs (- n 2))) (* 3 (f-recurs (- n 3))))))

;; 1.12
; Edges are all 1, and each number inside the triangle is the sum of the two numbers above it
;; compute via recursive process.
(defn pascal
  ([row column] (if (or (= column 1) (= column row))
                  1
                  (+ (pascal (- row 1) (- column 1))
                     (pascal (- row 1) column)))))

;; 1.16

(defn square
  [x]
  (* x x))

; Design an iterative process version of
(defn fast-expt
  [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b (/ n 2)))
        :else (* b (fast-expt b (dec n)))))

(defn fast-expt-iter
  "Calculates the exponentation `n` of base `b` in a logarithmic number of steps, through an iterative process."
  ([b n] (fast-expt-iter b n 1))
  ([b n a]
   (cond (= n 0) a
         (even? n) (recur (square b) (/ n 2) a)
         :else (recur b (dec n) (* b a)))))

;; 1.17
(defn mult [a b]
  (if (= b 0)
    0
    (+ a (mult a (dec b)))))

(defn double [x]
  (* x 2))

(defn halve [x]
  (/ x 2))

;; 1.17
(defn fast-mult
  "This is like 1.16, it provides multiplication with only the `double`, `halve` and addition functions.
  Multiplies in a logarithmic number of steps."
  [a b]
  (cond (= b 0) 0
        (even? b) (double (fast-mult a (halve b)))
        :else (+ a (fast-mult a (dec b)))))

;; 1.18
(defn fast-mult-iter
  "Same multiplication as 1.17, but done with an iterative process."
  ([a b] (fast-mult-iter a b 0))
  ([a b r] (cond
             (= b 0) r
             (even? b) (recur (double a) (halve b) r)
             :else (recur a (dec b) (+ a r)))))

;; 1.19
(defn fib [n]
  (fib-iter 1 0 0 1 n))

;; Need to calculate p' and q'
;;
;; b' = bp + aq
;; a' = bq + aq + ap

;; b'' = (bp + aq)p + (bq + aq + ap)q
;;     = bp^2 + aqp +aqp + aq^2 + bq^2
;;     = bp^2 + bq^2 + aq^2 + 2aqp
;;     = b(p^2 + q^2) + a(q^2 + 2qp)

;; a'' = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;;     = bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2
;;     = ap^2 + 2aq^2 + bq^2 + 2bpq + 2apq
;;     = a(p^2 + 2q^2 + 2pq) + b(q^2 + 2pq)
;;
(defn fib-iter [a b p q c]
  (cond (= c 0) b
        (even? c)
        (recur a b
               (+ (square p) (square q))
               (+ (square q) (* 2 q p))
               (/ c 2))
        :else (recur
               (+ (* b q) (* a q) (* a p))
               (+ (* b p) (* a q))
               p
               q
               (- c 1))))

;; 1.22
(defn divides? [a b]
  (= (rem b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (recur n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn timed-prime-test [n]
  (print (str "*** " (time (prime? n)))))


;; We haven't been introduced do blocks or looping constructs yet, so
;; adding a parameter purely to call side-effects. This is *very ugly*
(defn search-for-primes
  ([a b] (search-for-primes
          (if (= (rem a 2) 0)
            (inc a)
            a)
          b
          nil))
  ([a b _] (if (<= a b)
             (recur (+ a 2) b (timed-prime-test a))
             nil)))
