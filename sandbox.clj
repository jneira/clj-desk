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
