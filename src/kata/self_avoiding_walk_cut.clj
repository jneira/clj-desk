(ns self-avoiding-walk)

(def ^:dynamic n 4)

(defn dir [c]
  (if (= c 0) [1] (if (= c n) [-1] [1 -1])))

(defn dirs [[x y]]
  (concat (for [i (dir x)] [(+ x i) y])
          (for [j (dir y)] [x (+ y j)])))

(defn valid? [n prevs]
  (not-any? #{n} prevs))

(defn explore [[p & bcs :as a]]
  (let [ds (filter #(valid? % a) (dirs p))]
    (map #(cons % a) ds)))

(defn solution? [[p & _]]
  (= p [n n]))

(defn walk
  ([] (walk [[0 0]]))
  ([nodes]
     (if (solution? nodes) [nodes]
         (mapcat #(walk %1) (explore nodes)))))

(defn count-walks [m]
  (binding [n (- m 1)] (count (walk))))

