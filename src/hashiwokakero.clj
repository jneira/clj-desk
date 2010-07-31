(ns hashiwokakero
  (:require (clojure.contrib [combinatorics :as comb])))

(defn in-range? [vec coords]
  (boolean
   (reduce #(and
             (vector? %1)
             (> (count %1) %2)
             (%1 %2))
           vec coords)))

(defn backtracking [solution? explore node]
  (if (solution? node) [node]
      (mapcat
       #(backtracking solution? explore %1)
       (explore node))))

(def max-bridges 8)
(def directions
  {:north [-1,0] :east [0,1] :south [1,0] :west [0,-1]})

(defstruct node :panel :islands)
(defstruct coords :x :y)
(defstruct bridge :direction :value)
(defstruct island :coords :bridges)

(defn get-value [panel {:keys [x y]}]
  (get-in panel [x y]))

(defn init-panel [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (vec (map vec (interpose  (replicate length 0) with0)))))

(defn init-islands [p]
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (get-in p [x y]) 0)]
    (struct island (struct coords x y))))

(defn get-bridges
  ([vals] (get-bridges (keys directions) vals))
  ([dirs vals] (map #(struct bridge %1 %2) dirs vals)))

(defn init-bridges [maxTotal maxByBridge]
  (map  #(get-bridges %1)
        (filter #(= maxTotal (apply + %1))
                (map #(filter (complement zero?) %1)
                     (comb/selections (range 0 (inc maxByBridge)) 4)))))

(defn init-root-node [p]
  (let [panel (init-panel p)
        islands (init-islands panel)]
    (struct node panel islands)))

(defn move [{:keys [x y]} direction]
  (let [dir (when (keyword? direction)
               (directions direction))]
    (apply struct coords (map + [x y] dir))))

(defn num-bridges [panel island]
  (let [mv&get #(get-value panel (move island %1))
        cells (map mv&get (keys directions))]
    (apply + (remove nil?  cells))))

(defn left-bridges [panel island]
  (+ (get-value panel island) (num-bridges panel island)))

(defn fit-bridge [panel island bridge-val]
  (let [new-bridges
        (- (left-bridges panel island) bridge-val)]
    (>= new-bridges 0)))

(defn next-bridge-step [cell bridge]
  (let [dir (:direction bridge)]
    (move cell dir)))

(defn extend-bridge [panel cell bridge]
  (letfn
      [(step [prevCell acc]
             (let [val (:value bridge)
                   nextCell (move prevCell (:direction bridge))
                   nextVal (get-value panel nextCell)]
               (println val nextCell nextVal acc)
               (cond
                (nil? nextVal)    :out-of-range
                (< nextVal 0) (if (= cell prevCell)
                                  :cross-other-bridge
                                (if (= nextVal val)
                                  :match-prev-bridge
                                  :mismatch-prev-bridge))
                (> nextVal 0) (if (fit-bridge panel nextCell val)
                                acc
                                  :island-full)
                (= nextVal 0)  (recur nextCell (conj acc nextCell)))))]
    (step cell [])))

(defn put-bridges [panel island bridges]
  (let [exts (map #(extend-bridge panel island %1) bridges)]
    (when (not-any? keyword? exts)
      (reduce  ))))

(defn valid-bridges? [panel island bridges]
  nil)


(def node (init-root-node [[0 2 0] [0 0 0] [2 2 2] [0 2 0]]))

