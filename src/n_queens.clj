(ns n-queens
  (use clojure.math.combinatorics))

(def n 8)
(def all (permutations (range n)))
(defn without-diagonal [xs]
  (every? #(apply distinct? %)
          [(map-indexed + xs)
           (map-indexed #(- %2 %) xs)]))
(def sols 
  (filter without-diagonal all))

(defn pprint [t]
  (doseq [i (range n)]
    (apply println
           (for [j (range n)]
             (if (= j (nth t i)) \D  \_)))))
