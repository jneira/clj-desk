(ns mahalanobis)

(defn matrix? [m]
  (and (coll? m) (every? coll? m)))

(defn trans [x]
  (if (matrix? x)
    (apply map vector x)
    (map vector x)))

(defn div* [m k]
  (for [v m] (map #(/ % k) v)))

(defn sub [v w]
  (if (and (matrix? v) (matrix? w))
    (map sub v w)
    (map - v w)))

(defn mul
  ([m n]
     (let [[m n] (map #(if (coll? (first %)) % [%]) [m n])
           [[mi mj] [ni nj]]
             (map #(do [(count %) (count (first %))]) [m n])
           s (for [v m w (trans n)]
                         (apply +  (map * v w)))]
       (cond
        (not= mj ni)
        (throw (IllegalArgumentException. "error in matrix dims"))
        (= mi nj 1) (first s)
        (= mi 1) s
        :else (partition nj s))))
  ([m n & ms]
     (apply mul (mul m n) ms)))

(defn mean [v]
  (if (matrix? v)
    (map mean (trans v))
    (/ (apply + v) (count v))))

(defn center [m]
  (trans
   (for [v (trans m)]
     (map - v (repeat (mean v))))))

(defn cov [m]
  (let [xc (center m)
        N (count xc) ]
    (div* (mul (trans xc) xc) (- N 1))))

(defn sqrt [x]
  (cond (matrix? x) (map sqrt x)
        (vector? x) (map #(Math/sqrt %) x)
        :else (Math/sqrt x)))

(defn inverse [m])

(defn mahalanobis-distance [x & {:keys [y W centroid]}]
  (let [y (or y x)
        W (or W (inverse (cov y)))
        centroid (or centroid (map mean (trans y)))
        x-centroid (sub x centroid)
        dist #(sqrt (mul % W (trans %)))]
    (println x-centroid) (println W)
    (if (matrix? x)
      (map dist x-centroid)
      (dist x-centroid)) ))

(def m [[2 2] [2 5] [6 5] [7 3] [4 7]
        [6 4] [5 3] [4 6] [2 5] [1 3]])
(def n [[6 5] [7 4] [8 7] [5 6] [5 4]])
