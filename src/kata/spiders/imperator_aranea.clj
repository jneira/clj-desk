(ns kata.spiders.imperator-aranea)

(defn init-world [n]
  (let [r (range n)]
   {:size n
    :web (vec (for [x r] (vec (for [y r] (atom (if (= x y) 1 0))))))
    :spiders (vec (for [x r] (atom [x x])))}))

(declare world)

(defn mv [coord dx limit]
  (let [n (+ coord (* dx (rand-nth [-1 1])))]
    (cond (< n 0) 1 (>= n limit) (- limit 2)
          :else n)))

(defn move-to [point dir limit]
  (map #(mv % %2 limit) point dir))

(defn move-world []
  (let [{:keys [size web spiders]} world
        rnd #(rand-nth [[0 1] [1 0]])]
    (map #(let [new-pos (swap! % move-to (rnd) size)]
            (swap! (get-in web new-pos) inc))
         spiders)))

(defn pprint [{:keys [size web spiders]}]
  (doseq [x web] (println (map deref x)))
  (println (map deref spiders)))

(defn move-forever [n]
  (binding [world (init-world n)]
    (while (= "" (read-line))
      (pprint world)
      (time (dorun (move-world))))))
