(ns hashiwokakero
  (:require (clojure.contrib [combinatorics :as comb])))

(defn assocn [coll coords newVal]
  (reduce ) )

(defn in-range? [vec coords]
  (boolean
   (reduce #(and
             (vector? %1)
             (> (count %1) %2)
             (%1 %2))
           vec coords)))

(defn squared? [p]
  (let [length (count p)
        lengths (map count p)]
    (apply = length lengths)))

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

(defn init-panel [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (interpose (replicate length 0) with0)))

(defn init-islands [p]
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (nthn p [x y]) 0)]
    (struct island (struct coords x y))))

(defn get-bridges
  ([vals] (get-bridges (keys directions) vals))
  ([dirs vals] (map #(struct bridge %1 %2) dirs vals)))

(defn init-bridges [maxTotal maxByBridge]
  (map  #(get-bridges %1)
        (filter #(= maxTotal (apply + %1))
                (map #(filter (complement zero?) %1)
                     (comb/selections (range 0 (inc maxByBridge)) 4)))))

(defn valid-bridges? [panel island bridges]
  nil)

(defn init-root-node [p k]
  (let [panel (init-panel p)]))

(defn next-bridge-step [bridge cell]
  (let [dir (directions (:direction bridge))]
    (map +  dir cell)))

(defn fit-bridge [panel cell val]
  )

(defn extend-bridge [panel cell bridge]
  (letfn
      [(step [prevCell]
             (let [val (:value bridge)
                   nextCell (next-bridge-step bridge prevCell)
                   nextVal (get-in panel [(:x nextCell) (:y nextCell)])]
               (cond
                ((complement in-range?) panel nextCell) :out-of-range
                (< nextVal 0) (if (= cell prevCell)
                                :cross-other-bridge
                                (if (= nextVal val)
                                  :match-prev-bridge
                                  :mismatch-prev-bridge))
                (> nextVal 0) (if (fit-bridge panel nextCell val)
                                nextCell
                                :island-full)
                (= nextVal 0) (recur nextCell))))]
    (step cell)))





