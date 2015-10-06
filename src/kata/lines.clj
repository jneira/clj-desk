(ns lines)

(def s "Four score and seven years ago our fathers brought forth upon this continent a new nation conceived in liberty and dedicated to the proposition that all men are created equal")

(defn words [s] (re-seq #"\w+" s))
(defn unwords [xs] (apply str (interpose " " xs)))
(defn lines [s] (re-seq #"^\n+" s))
(defn unlines [xs] (apply str (apply concat (interpose ["\n"] xs))))

(defn split-word-every [n w]
  (map #(apply str %)
       (partition-all n w)))

(defn split-each-word-every [n ws]
  (mapcat #(split-word-every n %) ws))

(defn add-or-split [n ws w]
  (let [x (count w) y (apply + (map count ws))]
    (if (< (+ x y) n) [(into ws [" " w])]
        [ws [w]])))

(defn split-words-up-to [n [h & t]]
  (reduce (fn [ws w]
            (concat (butlast ws)
                    (add-or-split n (last ws) w)))
          [[h]] t))

(defn warp [n ws]
  (->> ws words
       (split-each-word-every n)
       (split-words-up-to n)
       unlines))
