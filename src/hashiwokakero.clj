(ns hashiwokakero
  (:require (clojure.contrib [combinatorics :as comb])))

(defn seq-not-empty? [arg]
  (and (seq? arg)) ((comp not empty?) arg))

(defn backtracking [solution? explore node]
  (if (solution? node) [node]
      (mapcat
       #(backtracking solution? explore %1)
       (explore node))))

(def max-bridges 8)
(def directions
  {:north [-1,0] :east [0,1] :south [1,0] :west [0,-1]})

(defstruct Node :panel :islands)
(defstruct Coords :x :y)
(defstruct Bridge :direction :value)
(defstruct Island :coords :bridges)

(defn get-value [panel {:keys [x y]}]
  (get-in panel [x y]))

(defn move [{:keys [x y]} direction]
  (let [dir (when (keyword? direction)
               (directions direction))]
    (apply struct Coords (map + [x y] dir))))

(defn num-bridges [panel island]
  (let [mv&get #(get-value panel (move island %1))
        cells (map mv&get (keys directions))]
    (apply + (remove nil?  cells))))

(defn left-bridges [panel island]
  (+ (get-value panel island) (num-bridges panel island)))

(defn fit-bridge [panel island bridge-val]
  (let [rest-bridges
        (- (left-bridges panel island) (Math/abs bridge-val))]
    (comment println (left-bridges panel island)
             "-" (Math/abs bridge-val) "=" rest-bridges)
    (>= rest-bridges 0)))

(defn extend-bridge [panel cell bridge]
  (comment println "\n" "extend-bridge:" bridge)
  (loop [prevCell cell acc []]
             (let [val (:value bridge)
                   nextCell (move prevCell (:direction bridge))
                   nextVal (get-value panel nextCell)]
               (comment println "prev:" prevCell
                        " next:" nextCell nextVal)
               (cond
                (nil? nextVal)    :out-of-range
                (< nextVal 0) (if (= cell prevCell)
                                (if (= (Math/abs nextVal) val)
                                  :match-prev-bridge
                                  :mismatch-prev-bridge)
                                :cross-other-bridge)
                (> nextVal 0) (if (fit-bridge panel nextCell val)
                                 acc
                                  :island-full)
                (= nextVal 0)  (recur nextCell
                                      (conj acc [nextCell val]))))))

(def valid-bridge-extend-results #{:match-prev-bridge})

(defn valid-extend-bridge? [extend]
  (or (not (keyword? extend))
         (some #{extend} valid-bridge-extend-results)))

(defn extend-bridges [panel coords bridges]
  (map #(extend-bridge panel coords %1) bridges))

(defn put-bridges [panel coords bridges]
  (let [exts (extend-bridges panel coords bridges)
        woutkeys (filter (comp not keyword?) exts)
        laying (fn [p ext]
                 (assoc-in p (vals (ext 0)) (- (ext 1))))]
    (reduce laying panel (apply concat woutkeys))))

(defn get-bridges
  ([vals] (get-bridges (keys directions) vals))
  ([dirs vals] (map #(struct Bridge %1 %2) dirs vals)))

(defn valid-bridges? [panel coords bridges]
  (let [exts (extend-bridges panel coords bridges)
        sum-bridges-vals (apply + (map (comp second first)
                                   (filter (comp not keyword?) exts)))]
    (and (fit-bridge panel coords sum-bridges-vals)
         (every? valid-extend-bridge? exts))))

(defn filter-bridges [panel {:keys [coords bridges]}]
  (let [filtered (filter
                  #(valid-bridges? panel coords %1) bridges)]
    (struct Island coords filtered)))

(defn init-panel [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (vec (map vec (interpose  (replicate length 0) with0)))))

(defn pred-value [pred]
   #(pred (:value %1)))

(defn init-bridges [maxTotal maxByBridge]
  (let [all (comb/selections (range 0 (inc maxByBridge)) 4)
        max (filter #(= maxTotal (apply + %1)) all)
        brs (map  #(get-bridges %1) max)
        wout0 (map #(filter (pred-value (complement zero?))%1) brs)] 
    wout0))

(defn init-islands [p]
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (get-in p [x y]) 0)]
    (let [xy (struct Coords x y)
          bridges (init-bridges (get-value p xy) 2)
          valid (filter #(valid-bridges? p xy %1) bridges)
          sorted (sort-by count valid)]
      (struct Island xy sorted))))

(defn init-root-node [p]
  (let [panel (init-panel p)
        islands (init-islands panel)
        sorted (sort-by (comp count :bridges) islands)]
    (struct Node panel sorted)))

(defn valid-node? [{:keys [panel islands]}]
  (and (seq-not-empty? islands)
       (every? (comp seq-not-empty? :bridges) islands)))

(defn explore
  ([vnodes n] (explore ((vec vnodes) n))) 
  ([node]
     (if ((comp not valid-node?) node) []
         (let [{:keys [panel islands]} node
               filtered (filter (comp not empty? :bridges)
                         (map #(filter-bridges panel %1) islands))
               sorted (sort-by (comp count :bridges) filtered)
               {:keys [coords bridges]} (first sorted)
               panels (map #(put-bridges panel coords %1) bridges)]
           (map #(struct Node %1 (rest sorted)) panels)))))

(defn exploren [node n]
  (if (seq? n)
    (reduce explore [node] n)
    (iterate #(explore %1 n) [node])))

(def panel [[0,0,0,0,0,0,1],
            [4,0,4,0,0,3,0],
            [0,0,0,1,0,0,3],
            [4,0,0,0,0,0,0],
            [0,0,0,3,0,4,0],
            [0,0,0,0,0,0,0],
            [3,0,3,0,0,0,3]])

(def mynode (init-root-node panel))

(def p (:panel mynode))
(def iss (:islands mynode))
(def is (last iss))
(def c {:x 4 :y 12})
(def brs (:bridges is))

