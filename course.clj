(ns course
  [:use book-list clojure.contrib.seq-utils])

(defn print-book
  "Prints out information about a book."
  [ {:keys [title authors price] :as book } ]
  (println "Title:" title)
  (let [[first second & more] authors]
    (println "  Author: " (comma-sep
                           (filter seq [first
                                        second
                                        (when more "et. al.")]))))
  (println "  Price:" (str price))
  (println "  Raw:" (pr-str book)))
    

(defn print-books [& books]
  (let [f (flatten books)] 
    (dorun ( map print-book f))
    (print   "\n=============\n")
    (println "Printed information on"
             (count f)
             "books.")))

(def cities [{:city "Vienna" :temp 55} {:city "Krakow" :temp 52} {:city "Pune" :temp 85} {:city "Houston" :temp 57}])

;   1. Write a program that outputs the average temperature of the above cities rounded to 2 decimal places.
;   2. Since two of the four cities are in Europe, also add a function to convert temperatures between Fahrenheit to Celsius.

(defn avg-temp [cities]
     (.setScale
      (/ (apply + (map :temp cities)) (BigDecimal. (count cities)))
      2 (java.math.RoundingMode/CEILING)))

(defn temp-average [cities]
  (with-precision 2 :rounding UNNECESSARY
                  (.decimalValue (/ (apply + (map :temp cities)(count cities) )))))

(defn cº-to-fª [c] (+ 32 (* 1.8 c)))
(defn fª-to-cº [f] (/ (- 32 f) 1.8))
