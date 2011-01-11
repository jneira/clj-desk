(ns roman-numerals-test
  (:use roman-numerals clojure.test))

(deftest conversion-from-int-to-roman
  (defn to-roman [n] "I")
  (testing "1 must be I"
    (is (= (to-roman 1) "I"))))

