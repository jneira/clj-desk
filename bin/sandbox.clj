;Code competition by fogus

(defn filter-collecting [predicate collector & lists]
  (lazy-seq
   (loop [lists lists out []]
     (if (empty? (first lists))
       (reverse out)
       (let [heads (map first lists)]
         (if (apply predicate heads)
           (recur (map rest lists) (cons (apply collector heads) out))
           (recur (map rest lists) out)))))))
 
;; usage:
(filter-collecting
 (fn [x y] (< x y))
 (fn [x y] (+ x y))
 '(1 7 3 9)
 '(5 5 5 5))
;; ==> (6 8)
 
;Jarkko Oranen
(defn filter-collecting [c f & seqs]
  (remove nil?
          (apply map (fn [& args]
                       (when (apply c args)
                         (apply f args)))
                 seqs)))

;Christophe Grand
;my personal favorite
(defn filter-collecting [predicate collector & lists]
  (for [v (apply map vector lists)
          :when (apply predicate v)]
    (apply collector v)))

;Chris Houser
(defn filter-collecting [p c & l]
  (map #(apply c %)
       (filter #(apply p %)
               (apply map vector l))))
;Laurent Petit
(let [x (Object.)]
   (defn filter-collecting [p c & l]
      (remove #(identical? % x)
        (apply map #(if (apply p %&) (apply c %&) x) l))))

; AND
(defn filter-collecting [p c & l]
  (apply mapcat
         #(when (apply p %&) [(apply c %&)]) l))

;Jonathan Smith
(defn filter-collecting [p c & seqs]
  (let [fun #(if (apply p %1)
               (conj! %2 (apply c %1))
               %2)]
    (loop [hs (map first seqs)
           ts (map rest seqs)
           result (transient [])]
      (if (next (first ts))
        (recur (map first ts)
               (map rest ts)
               (fun hs result))
        (persistent! (fun hs result))))))

;more clojure code snippets
(defn mapreduce 
      ([fnrd fnmap lst] (mapreduce fnrd fnmap (first lst) (rest lst)))
      ([fnrd fnmap id lst]
     	    (reduce fnrd id (map fnmap lst))))


(defn mapreduce [fnrd fnmap lst] 
  (apply (comp (partial reduce fnrd) (partial map fnmap)) (list lst) ))

;;matchure example
(use 'matchure)
(defn-match fib
  ([0] 1)
  ([1] 1)
  ([?n] (+ (fib (dec n)) (fib (dec (dec n))))))


(defn squared? [p]
  (let [length (count p)
        lengths (map count p)]
    (apply = length lengths)))


(defn in-range? [vec coords]
  (boolean
   (reduce #(and
             (vector? %1)
             (> (count %1) %2)
             (%1 %2))
           vec coords)))

(defn ns-unmap-all
"Unmap all the symbols (except 'ns' and 'in-ns')."
([] (ns-unmap-all *ns*))
([ns] (map #(ns-unmap ns %) (keys (ns-map ns)))))

;;sols for problem:
;;convert a collection [1 2 0 1 2 3 0 1 2 3 0 0 1 2] into
;;partitions like:
;;((1 2) (0 1 2 3) (0 1 2 3) (0) (0 1 2))
;; http://groups.google.com/group/clojure/browse_thread/thread/f1593f492759e66b

(defn unfold-1 [unspool finished x]
  (lazy-seq
    (when-not (finished x)
      (let [[a y] (unspool x)]
        (cons a (unfold unspool finished y)))))) 

 
(defn unfold
  "(unfold seed grow) use the seed and a function grow that returns an
element and another seed to creates a lazy seq. The seq is stopped the grow function returns nil."  
  ([grow seed]
     (lazy-seq
      (if-let [[elt next-seed] (grow seed)]
        (cons elt (unfold grow next-seed)))))
  ([grow  finished? seed]
     (unfold #(when (not (finished? %)) (grow %)) seed))) 

(defn partition-starting-every-1 [f coll]
  (->> [nil coll]
       (iterate (fn [[p [x :as s1]]]
                  (let [[n s2] (split-with (complement f) (next s1))]
                    (when (seq s1)
                      [(cons x n) s2]))))
       (map first)
       next
       (take-while identity)))

(defn partition-starting-every-2 [f coll]
  (let [pb (partition-by #(and (f %) (Object.)) coll)]
    (->> (map (fn [[a :as as] [b :as bs]]
                (cond
                  (nil? as) (when-not (f b) bs)
                  (and (f a) (f b)) as
                  (f a) (concat as bs)))
              (cons nil pb) pb)
         (remove nil?))))

(defn partition-starting-every-chouser
  "Partition the sequence starting each partition when the f is true."
  [f coll]
  (lazy-seq
    (when-let [[x & xs] (seq coll)]
      (let [[a b] (split-with (complement f) xs)]
        (cons (cons x a) (partition-starting-every f b))))))

(defn partition-starting-every-cgrand
 "Partition the sequence starting each partition when the f is true."
 [f coll]
 (map #(map first %)
   (partition-by second
     (map vector coll
       (rest (reductions #(if (f %2) (inc %1) %1) 0 coll)))))) 