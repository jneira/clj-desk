(ns floyd-warshall
  (:require [clojure.java.io :as io]))

(set! *warn-on-reflection* true)

(defn words [s] (re-seq #"\w+" s))

(defn read-data [name]
  (let [lines (line-seq (io/reader name))]
    (map #(map read-string (words %)) lines)))

(defn edges-cost [[h & triples]]
  (let [kvs (mapcat (fn [[u v c]] [[u v] c])
                    (rest triples))]
    [h (apply hash-map kvs)]))

(def inf 50000)

(defn init [n ec]
  (let [init (for [i (range n) j (range n)]
               (if (= i j) 0 (get ec [i j] inf)))]
    (int-array (* n n) init)))

(defn fw
  ([n a]
     (let [m (- n 1)] (fw n a [0 0 0] [m m m] )))
  ([n ^ints a [ii ij ik] [li lj lk]]
     (loop [i ii j ij k ik]
       (let [ij (+ (* j n) i)
             aik (aget a (+ (* k n) i))
             akj (aget a (+ (* j n) k))]
         (aset a ij (min (aget a ij) (+ aik akj)))
         (cond (= k lk) a
               (= i li) (recur 0 0 (+ 1 k))
               (= j lj) (recur (+ i 1) 0 k)
               :else    (recur i (+ j 1) k))))))

(defn sol [name]
  (let [[[n _] ecs] (edges-cost (read-data name))
        i (init n ecs)]
    (fw n i)))

(defn has-neg-cycle [n a]
  (let [diag (map #(aget a (+ (* n %) %)) (range n))]
    (some #(< % 0) diag)))

(defn shortest [a] (apply min a))

(def solutions {"g1.txt" 0})
