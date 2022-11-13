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

<<<<<<< HEAD
=======
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
   (cond ((= n 0) 1
          ((even? n) (square (fast-expt b (/ n 2)))
           :else (* b (fast-expt b (dec n)))))))
>>>>>>> origin/main
