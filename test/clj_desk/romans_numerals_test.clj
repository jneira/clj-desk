(ns roman-numerals-test
  (:use roman-numerals clojure.test))



(deftest conversion-from-int-to-roman

  (defn repeat-str [n s]
    (apply str (repeat n s)))
  
  (defn to-roman [n]
    (cond
     (> n 9) (str "X" (to-roman (- n 10)))
     (= n 9) "IX"
     (> n 4) (str "V" (repeat-str (mod n 5) "I"))
     (= n 4) "IV"
     (< n 4) (repeat-str n "I")))
  (are [x y]  (= (to-roman x) y)
       1 "I" 2 "II" 3 "III" 4 "IV" 5 "V"
       6 "VI" 9 "IX" 10 "X" 14 "XIV" 19 "XIX"
       39 "XXXIX" 40 ))

(run-tests)
