(ns roman-numerals-test
  (:use roman-numerals clojure.test))

(deftest conversion-from-int-to-roman
  (defn to-roman [n] (case n 1 "I" 2 "II"))
  (testing "1 must be I"
    (is (= (to-roman 1) "I")))
  (testing "2 must be II"
    (is (= (to-roman 2) "II"))))

(run-tests)
