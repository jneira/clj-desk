(ns kata.spiders.immutabilis-aranea)

(defn init-world [n]
  (let [r (range n)]
   {:size n
    :web (vec (for [x r] (vec (for [y r] (if (= x y) 1 0)))))
    :spiders (vec (for [x r] [x x]))}))

(defn mv [c d l]
  (let [n (+ c (* d (rand-nth [-1 1])))]
    (cond (< n 0) 1 (>= n l) (- l 2)
          :else n)))

(defn move-to [p d l]
  (map #(mv % %2 l) p d))

(defn move-world [w]
  (let [s (:size w) rnd #(rand-nth [[0 1] [1 0]])]
    (reduce
     #(let [w (update-in % [:spiders %2] move-to (rnd) s)]
          (update-in w (cons :web (get-in w [:spiders %2])) inc))
     w (range s))))

(defn move-forever [n]
  (iterate move-world (init-world n)))

;; pretty print some worlds
(comment
  (doseq [w (take 5 (move-forever 5))]
    (doseq [i (:web w)]
      (println i))
    (println (:spiders w))
    (newline)))

