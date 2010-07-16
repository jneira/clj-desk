(ns hashiwokakero
  (:require (clojure.contrib [combinatorics :as comb])))

(defn nthn [coll coords]
  (reduce nth coll coords))

(defn backtracking [solution? explore node]
  (if (solution? node) [node]
      (mapcat
       #(backtracking solution? explore %1)
       (explore node))))

(defonce max-bridges 8)
(defonce directions
  {:north [0,-1] :east [1,0] :south [0,1] :west [-1,0]})

(defstruct puzzle-node :panel :islands)
(defstruct coords :x :y)
(defstruct bridge :direction :value)
(defstruct island :coords :bridges)


(defn squared? [p]
  (let [length (count p)
        lengths (map count p)]
    (apply = length lengths)))

(defn init-puzzle [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (interpose (replicate length 0) with0)))

(defn get-islands [p]
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (nthn p [x y]) 0)]
    (struct island (struct coords x y))))

(defn valid-bridges? [panel island bridges]
  nil)

(defn get-bridges
  ([vals] (get-bridges (keys directions) vals))
  ([dirs vals] (map #(struct bridge %1 %2) dirs vals)))

(defn init-bridges [maxTotal maxByBridge]
  (map  #(get-bridges %1)
        (filter #(= maxTotal (apply + %1))
                (comb/selections (range 0 (inc maxTotal)) 4))))

(defn limit-num-bridges? [limit] nil)
