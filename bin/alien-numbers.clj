(ns alien-numbers
  (:require (clojure.contrib [combinatorics :as comb])))

(defn decimal-to-lang
  "Converts decimal-num to the equivalent number in lang as a string.
  The algorithm was a translation of the algorithm by Rodrigo Menezes
  in C# that he posted in the programming praxis site."
  [decimal-num lang]
  (let [lang-radix (count lang)]
    (loop [decimal-value decimal-num lang-num []]
      (if (>= 0 decimal-value)
        (apply str lang-num)
        (recur (int (/ decimal-value lang-radix))
               (concat [(nth lang (mod decimal-value lang-radix))] lang-num))))))

(defn digit-index
  [digit lang]
  (loop [i 0]
    (if (= digit (nth lang i))
      i
      (recur (inc i)))))

(defn lang-to-decimal
  [alien-num lang]
  (let [radix (count lang)
        ralien-num (reverse alien-num)]
    (loop [i 0 decimal-num 0 product 1]
      (if (= i (count ralien-num))
        decimal-num
        (recur (inc i) (+ decimal-num (* (digit-index (nth ralien-num i) lang) product))
               (* radix product))))))

(defn convert-num
  "Convert alien-num which is in source-lang into the same number in target-lang"
  [alien-num source-lang target-lang]
  (decimal-to-lang (lang-to-decimal alien-num source-lang) target-lang))

(defn generate-nums [lang]
  (apply concat
         (iterate #(concat
                    (comb/cartesian-product %1 lang)) lang)))

(defn decimal-to-lang2 [dec lang]
  (take dec (generate-nums lang)))
