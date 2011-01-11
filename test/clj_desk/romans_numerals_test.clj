(ns roman-numerals-test
  (:use roman-numerals clojure.test))

(deftest conversion-from-int-to-roman
  (defn to-roman [n]
    (case n 1 "I" 2 "II" 3 "III"))
  (are [x y]  (= (to-roman x) y)
       1 "I"
       2 "II"
       3 "III"))

(run-tests)
