(ns kata.fizzbuzz
  (:use clojure.test))

(defn div-by? [x y]
  (or (= 0 (mod x y))))

(defn include? [x y]
  (or (= x y)
      (= (mod x 10) y)
      (and (> x y)
           (recur (quot x 10) y))))

(defn div-by-or-include? [x y]
  (some true?
        ((juxt div-by? include?) x y)))

(defn fizz-buzz []
  (for [x (range 1 100)
        :let
        [s (str
            (when (div-by-or-include? x 3)
              "fizz")
            (when (div-by-or-include? x 5)
              "buzz"))]]
    (if (seq s) s x)))

(deftest let-test-fizz-buzz
    (are [x y] (= y ((vec (fizz-buzz))
                     (dec x)))
         1 1
         3 "fizz"
         31 "fizz"
         5 "buzz"
         52 "buzz"
         15 "fizzbuzz"
         35 "fizzbuzz")
         53 "fizzbuzz")

(defn fizzbuzz [n]
  (let [m #(= 0 (mod % %2))
        e #(some #{%2} (str %))
        r #(str (when (or (m % 3) (e % \3)) "fizz")
                (when (or (m % 5) (e % \5)) "buzz"))]
    (map #(if (seq (r %)) (r %) %) (range 1 n))))
