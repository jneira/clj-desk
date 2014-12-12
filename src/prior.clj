(ns prior)

(def ex [{:p 1} {:p 1} {:p 3} {:p 3} {:p 3}
         {:p 5} {:p 5} {:p 8} {:p 8} {:p 8}])

(defn max-prob [xs]
  (/ 1 (apply + (map #(/ 1 (:p %)) xs))))

(defn rnd-idx [xs]
  (let [mp (max-prob xs)]
    (loop [r (rand) i 0
           [{p :p} & t] xs]
      (let [ap (- r (/ mp p))]
        (if (<= ap 0) i
            (recur ap (inc i) t))))))

