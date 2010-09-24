(ns hashiwokakero.core 
  (:require (clojure.contrib [combinatorics :as comb]))
  (:use (hashiwokakero utilities pprint)))

;; data
(def DIRECTIONS
  {:north [-1,0] :east [0,1] :south [1,0] :west [0,-1]})

(defstruct Node :panel :islands)
(defstruct Coords :x :y)
(defstruct Bridge :direction :value)
(defstruct Island :coords :bridges)

;;functions
(defn get-direction [dir]
  (if (keyword? dir)
    (DIRECTIONS dir) dir))

(defn cell-value [panel {:keys [x y]}]
  (get-in panel [x y]))

(defn move [{:keys [x y]} direction]
  (let [dir (get-direction direction)]
    (apply struct Coords (map + [x y] dir))))

(defn count-islands [p]
  (sum (map (comp count #(filter pos? %1)) p)))

(defn get-bridges [panel coords]
  (let [move&get
        #(struct Bridge %1
                 (abs (cell-value panel (move coords %1))))
        brs (map move&get (keys DIRECTIONS))]
    (remove (comp nil? :value) brs)))

(defn get-bridges-vals [panel coords]
  (map :value (get-bridges panel coords)))

(defn left-bridges [panel coords]
  (let [val (cell-value panel coords)
        sum-brs (abs (sum (get-bridges-vals panel coords)))]
    (- val sum-brs)))

(defn rest-bridges [panel coords br-val]
  (- (left-bridges panel coords) (abs br-val)))

(defn fit-bridge?
  [panel coords bridge-val]
  (let [rest-bridges (rest-bridges
                      panel coords bridge-val)]
    (>= rest-bridges 0)))

(defn row-from-cell [panel {:keys [x y]} direction]
  (let [[dx dy :as dir] (get-direction direction)
        hz? (zero? dx) rev? (some neg? dir)
        [row aux] (if hz? [(panel x) y]
                      [(map #(get %1 y) panel) x])
        rev (if rev? (reverse row) row)
        from (if rev? (- (count rev) aux 1) aux)]
    (subvec (vec rev) from)))

(defn indexed-row [row coords direction]
  (map vector (iterate #(move %1 direction) coords) row))

(defn inter-islands [panel cell direction]
  (let [subrow (row-from-cell panel cell direction)
        part (partition-by pos? subrow)
        bridge (apply (comp vec concat) (take 3 part))]
    (indexed-row bridge cell direction)))

;; validations on bridges
(defn out-of-range?
  [path dest]
  (or (empty? path) (<= dest 0)))

(defn prev-bridge? [path-vals]
  (and (seq path-vals) (every? neg? path-vals)))

(defn prev-bridge-match? [path-vals bridge-value]
  (and (prev-bridge? path-vals)
       (= bridge-value (abs (first path-vals)))))

(defn free-path? [path-vals dest]
  
  (and ((comp not out-of-range?) path-vals dest)
       (every? zero? path-vals)))

(defn get-path [panel coords
               {:keys [direction value]}]
  (let [path (inter-islands
             panel coords direction)]
    [path value]))

(defn valid-bridge? [panel row br-val]
  (let [dest (last row)
        path ((comp rest drop-last) row)
        path-vals (map second path)
        free-path? (free-path? path-vals (second dest))
        fit-bridge? (fit-bridge? panel (first dest) br-val)]
    (and free-path? fit-bridge?)))

(defn bridges-isolate-islands?
  [exts num-islands]
  (let [sum-brs-vals (sum (map second exts))
        sum-dest-vals (sum
                       (map (comp last last first) exts))]
      (and (>= sum-brs-vals sum-dest-vals)
               (> (count exts) num-islands))))

(defn valid-bridges? [panel coords bridges]
  (let [prevs (filter (comp pos? :value)
                      (get-bridges panel coords))
        exts (map #(map (partial get-path panel coords) %1) 
                  [bridges prevs])
        all-valid?
        (fn [] (every? #(valid-bridge?
                          panel (%1 0) (%1 1)) (first exts)))
        isolates? #(bridges-isolate-islands?
                    (apply concat exts) (count-islands panel))]
    (comment println (fit?) (all-valid?) (isolates?))
    (and (all-valid?) (not (isolates?)))))

(defn fit-bridges? [panel coords bridges]
  (let [sum-bridges (sum (map :value bridges))]
   (= (left-bridges panel coords)
      sum-bridges)))

(defn extend-bridge [panel cell {:keys [direction value]}]
  (let [path (inter-islands panel cell direction)]
    (map #(vector (first %1) value)
         ((comp rest butlast) path))))

(defn extend-bridges [panel coords bridges]
  (map (partial extend-bridge panel coords) bridges))

(defn put-bridges [panel coords bridges] 
  (let [exts (extend-bridges panel coords bridges)
        laying (fn [p ext]
                 (assoc-in p (vals (ext 0)) (- (ext 1))))]
    (reduce laying panel (apply concat exts))))

(defn create-bridges
  ([vals] (create-bridges (keys DIRECTIONS) vals))
  ([dirs vals] (map #(struct Bridge %1 %2) dirs vals)))

(defn filter-bridges [panel coords bridges]
  (filter (partial valid-bridges? panel coords) bridges))

(defn filter-island-bridges [panel {:keys [coords bridges]}]
  (let [filtered (remove empty?
                         (filter-bridges panel coords bridges))] 
    (struct Island coords filtered)))

(defn init-panel [p]
  (let [with0 (map #(interpose 0 %1) p)
        length (count (first with0))] 
    (vec (map vec (interpose
                   (replicate length 0) with0)))))

(defn init-bridges [max-total max-by-bridge]
  (let [all (comb/selections (range 0 (inc max-by-bridge)) 4)
        max (filter #(= max-total (sum %1)) all)
        brs (map create-bridges max)
        wout0 (map (partial remove (comp zero? :value)) brs)] 
    wout0))

(defn islands-coords [p]
  (println p)
  (for [x (range 0 (count p))
        y (range 0 (count (nth p x)))
        :when (> (get-in p [x y]) 0)]
    (struct Coords x y)))

(defn remove-bridges [dirs brs]
  (let [rm (remove #((set dirs)(:direction %1)) brs)] rm))

(defn init-islands [p]
  (for [coords (islands-coords p)
        :let
        [bridges (init-bridges (cell-value p coords) 2)
         valid (filter-bridges p coords bridges)
         rm (remove empty? (map #(remove-bridges
                                  [:north :west] %1) valid))]]
    (struct Island coords rm)))

(defn init-root-node [p]
  (let [panel (init-panel p)
        islands (init-islands panel)
        sorted (sort-by (comp count :bridges) islands)]
    (struct Node panel islands)))

(defn test-bridges [panel {:keys [coords bridges]}]
  (comment = (get-bridges-vals panel coords) ) true)

(defn valid-node? [{:keys [panel islands]}]
  (and  (no-empty-seq? islands)
        (not-any? #(empty? (:bridges %1)) (butlast islands))))

(defn remove-bridged-islands [p iss]
  (remove #(let [c (:coords %)]
             (zero? (left-bridges p c)))
          iss))

(defn filter-islands-bridges
  [{:keys [panel islands]}]
  (let [iss (remove-bridged-islands panel islands)
        [{:keys [coords bridges]} & rest]
        (map (partial filter-island-bridges panel)
             iss)]
    (cons (struct Island coords bridges) rest)))

(def history (atom []))
(defn init-history [] (swap! history (fn [_] [])))

(defn explore
  ([vnodes n] (explore ((vec vnodes) n))) 
  ([{:keys [panel islands] :as node}]
     (swap! history #(conj %1 node))
     (if ((comp not valid-node?) node) []
         (let [islands (filter-islands-bridges node)
               {:keys [coords bridges]} (first islands)
               panels (map (partial put-bridges panel coords)
                           bridges)]
           (map #(struct Node %1 (rest islands)) panels)))))

(defn all-islands-bridged? [node]
  (let [islands (map :coords (:islands node))
        sum-all-bridges
        (sum (map (partial left-bridges
                           (:panel node)) islands ))]
    (zero? sum-all-bridges)))

(defn solution? [node]
  (and (empty? (:islands node))
       (all-islands-bridged? node)))

(defn exploren [node n]
  (if (coll? n)
    (reduce explore [node] n)
    (exploren node (repeat n 0))))

(defn resolve-puzzle [panel]
  (let [root (init-root-node panel)
        solutions (backtracking solution? explore root)]
    (map :panel solutions)))
