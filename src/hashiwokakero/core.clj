(ns hashiwokakero.core 
  (:require (clojure.contrib [combinatorics :as comb]))
  (:use (hashiwokakero utilities)))

;; data
(def DIRECTIONS
  {:north [-1,0] :east [0,1] :south [1,0] :west [0,-1]})

(defstruct Node :panel :islands :adjacency)
(defstruct Coords :x :y)
(defstruct Bridge :direction :value)
(defstruct Island :coords :bridges)

;;functions
(defn get-value [panel {:keys [x y]}]
  (get-in panel [x y]))

(defn move [{:keys [x y]} direction]
  (let [dir (if (keyword? direction)
               (DIRECTIONS direction) direction)]
    (apply struct Coords (map + [x y] dir))))

(defn num-bridges [panel island]
  (let [move&get #(get-value panel (move island %1))
        cell-values (map move&get (keys DIRECTIONS))]
    (sum (remove nil? cell-values))))

(defn left-bridges [panel island]
  (- (get-value panel island)
     (abs (num-bridges panel island))))

(defn fit-bridge [panel island bridge-val]
  (let [rest-bridges
        (- (left-bridges panel island) (abs bridge-val))]
    (>= rest-bridges 0)))

(defn extend-bridge [panel cell bridge]
  (loop [prev-cell cell acc []]
    (let [val (:value bridge)
          next-cell (move prev-cell (:direction bridge))
          next-val (get-value panel next-cell)
          next-acc (conj acc [next-cell val])]
      (cond
       (nil? next-val) :out-of-range
       (< next-val 0) (if (= cell prev-cell)
                        (if (= (abs next-val) val)
                          (recur next-cell next-acc)
                          :mismatch-prev-bridge)
                        :cross-other-bridge)
       (> next-val 0) (if (fit-bridge panel next-cell val)
                        next-acc
                        :island-full)
       (= next-val 0) (recur next-cell next-acc)))))

(defn extend-bridges [panel coords bridges]
  (map (¬ extend-bridge panel coords) bridges))

(defn put-bridges [panel coords bridges] 
  (let [exts (butlast (extend-bridges panel coords bridges))
        woutkeys (remove keyword? exts)
        laying (fn [p ext]
                 (assoc-in p (vals (ext 0)) (- (ext 1))))]
    (reduce laying panel (apply concat woutkeys))))

(defn get-bridges
  ([vals] (get-bridges (keys DIRECTIONS) vals))
  ([dirs vals] (map #(struct Bridge %1 %2) dirs vals)))

(defn valid-bridges? [panel coords bridges]
  (let [exts (extend-bridges panel coords bridges)
        woutkeys (remove keyword? exts)
        bridges-vals (sum (map (º second first) woutkeys))]
    (and (fit-bridge panel coords bridges-vals)
         (not-any? keyword? exts))))

(defn filter-bridges [panel coords bridges]
  (filter (¬ valid-bridges? panel coords) bridges))

(defn filter-island-bridges [panel {:keys [coords bridges]}]
  (let [filtered (filter-bridges panel coords bridges)] 
    (struct Island coords filtered)))

(defn init-panel [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (vec (map vec (interpose (replicate length 0) with0)))))

(defn init-bridges [max-total max-by-bridge]
  (let [all (comb/selections (range 0 (inc max-by-bridge)) 4)
        max (filter #(= max-total (sum %1)) all)
        brs (map get-bridges max)
        wout0 (map (¬ remove (º zero? :value)) brs)] 
    wout0))

(defn islands-coords [p]
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (get-in p [x y]) 0)]
    (struct Coords x y)))

(defn init-islands [p]
  (for [coords (islands-coords p)
        :let
        [bridges (init-bridges (get-value p coords) 2)
         valid (filter-bridges p coords bridges)
         sorted (sort-by count valid)]]
  (struct Island coords sorted)))

(defn init-root-node [p]
  (let [panel (init-panel p)
        islands (init-islands panel)
        sorted (sort-by (º count :bridges) islands)]
    (struct Node panel sorted)))

(defn valid-node? [{:keys [panel islands]}]
  (and (no-empty-seq? islands)
       (every? (º no-empty-seq? :bridges) islands)))

(defn filter&sort-islands-bridges
  [{:keys [panel islands]}]
   (let [filtered
         (remove (º empty? :bridges)
                 (map (¬ filter-island-bridges panel)
                      islands))
         sorted (sort-by (º count :bridges) filtered)]
     sorted))

(defn explore
  ([vnodes n] (explore ((vec vnodes) n))) 
  ([{:keys [panel islands] :as node}]
     (if ((º not valid-node?) node) []
         (let [islands (filter&sort-islands-bridges node)
               {:keys [coords bridges]} (first islands)
               panels (map (¬ put-bridges panel coords) bridges)]
           (map #(struct Node %1 (rest islands)) panels)))))

(defn all-islands-bridged? [node]
  (let [islands (map :coords (:islands node))
        sum-all-bridges
        (sum (map (¬ left-bridges (:panel node)) islands ))]
    (zero? sum-all-bridges)))

(defn solution? [node]
  (and (empty? (:islands node)) (all-islands-bridged? node)))

(defn exploren [node n]
  (if (seq? n)
    (reduce explore [node] n)
    (iterate #(explore %1 n) [node])))

(defn resolve-puzzle [panel]
  (let [root (init-root-node panel)
        solutions (backtracking solution? explore root)]
    solutions))
