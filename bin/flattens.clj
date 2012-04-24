;;from clojure.contrib.seq-utils
;;'flatten' written by Rich Hickey,
;;see http://groups.google.com/group/clojure/msg/385098fabfcaad9b
(defn flatten [x]
  (filter (complement sequential?)
      (rest (tree-seq sequential? seq x))))

;aprox. translate from paul graham's on lisp to clojure
(defn flatten-ol [x]
     (letfn [(rec [x acc]
       (cond (not (seq? x)) (cons x acc)
             (empty? x) acc
              true (recur (first x) (rec (rest x) acc))))]
(lazy-seq (rec x nil))))

;remake with reduce
(defn flatten-ol2 [tree]
     (letfn [(rec [acc x]
        (if (not (seq? x))
          (concat acc [x])
          (reduce rec acc x)))]
       (lazy-seq (reduce rec () tree))))

; from twitters cgrand and fogus
 (defn flatten-cgrand [s] (remove seq? (tree-seq seq? seq s)))
 (defn flatten-fogus [x] (if (seq? x)
              (lazy-seq (apply concat (map flatten-fogus x)))(list x)))

(def bigtree (for [x (range 1 500)] (cons (range 1 5) (range 1 1000))))

;: force the whole tree
;: (btw it would be better to use (take 499 (iterate inc 1)) instead of (range 1 500)
;; because range returns an optimized seq
(dorun (flatten bigtree))

(time (dorun (flatten bigtree)))
;??
(time (dorun (flatten-ol bigtree)))
;??
(time (dorun (flatten-ol2 bigtree)))
;??
(time (dorun (flatten-cgrand bigtree)))
;??
(time (dorun (flatten-fogus bigtree)))
;??

