(ns hashiwokakero)

(defonce max-bridges 8)
(defonce dirs {:north [0,-1] :east [1,0] :south [0,1] :west [-1,0]})

(defstruct puzzle-node :panel :islands)


(comment defn nthn-old [coll coords] 
  (let [s (seq coords)
        next (nth coll (first s))]
    (if (= (count s) 1) next
        (recur next (rest coords)))))

(defn nthn [coll coords]
  (reduce nth coll coords))

(defn squared? [p]
  (let [length (count p)
        lengths (map count p)]
    (apply = length lengths)))

(defn init-puzzle [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (interpose (replicate length 0) with0)))

(defn init-islands [p]
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (-> p (nth x) (nth y)) 0)]
    [x,y]))

