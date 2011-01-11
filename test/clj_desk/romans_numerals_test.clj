(ns roman-numerals-test
  (:use roman-numerals clojure.test))



(deftest conversion-from-int-to-roman

  (defn repeat-str [s n]
    (apply str (repeat n s)))
  
  (defn to-roman [n]
    (cond
     (> n 4) (str "V"
                  (apply str (repeat (mod n 5) "I")))
     (= n 4) "IV"
     (< n 4) (apply str (repeat n "I"))     ))
  (are [x y]  (= (to-roman x) y)
       1 "I" 2 "II" 3 "III" 4 "IV" 5 "V"
       6 "VI" 9 "IX"))

(run-tests)
