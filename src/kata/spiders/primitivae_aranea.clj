(ns kata.spiders.primitivae-aranea)

(set! *warn-on-reflection* true)

(defn init-world [n]
  (let [r (range n)]
    {:size (int n)
    :web (to-array (for [x r] (int-array n (for [y r] (if (= x y) 1 0)))))
     :spiders (to-array (for [x r] (int-array 2 [x x])))}))

(declare world)

(defn mv [coord dx limit]
  (let [n (+ coord (* dx (rand-nth [-1 1])))]
    (cond (< n 0) 1 (>= n limit) (- limit 2)
          :else n)))

(defn move-to [^ints point ^ints dir limit]
  (dotimes [i (alength point)]
    (aset point i (mv (aget point i) (aget dir i) limit))))

(defn aupdate-in [v ^ints idxs f & args]
  (let [>>= apply]
    (>>= aset v `[~@idxs ~(>>= f (>>= aget v idxs) args)])))

(defn move-world []
  (let [{:keys [size web spiders]} world]
    (dotimes [i (alength spiders)]
      (let [spider (aget spiders i)
            rnd (int-array (rand-nth [[0 1] [1 0]]))]
        (move-to spider rnd size)
        (aupdate-in web spider inc)))))

(defn pprint [{:keys [size web spiders]}]
  (doseq [x web] (println (seq x)))
  (println (map seq spiders)))

(defn move-forever [n]
  (binding [world (init-world n)]
    (while (= "" (read-line))
      (comment pprint world)
      (time (dorun (move-world))))))
