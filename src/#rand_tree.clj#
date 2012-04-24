(ns rand-tree)

(defn seed []
  (let [r #(when (zero? (rand-int 2)) seed)]
    {:left (r) :right (r)}))

(def seed? fn?)
(def leave? nil?)
(def branch? map?)

(defn to-seq [tree]
  (letfn
   [(aux [stack]
         (if-let [s (seq stack)]
           (let [node (first s)
                 expand (if (seed? node) (node) node)
                 nxt #(aux (concat (vals expand)
                                   (rest s)))]
             (lazy-seq (cons expand (nxt))))
           '()))]
   (aux [tree])))

;; using standar tree-seq you can get a stackoverflow
(defn core-to-seq [root]
  (tree-seq branch? #(for [v (vals %)] (when v (v))) root))

(defn test []
  (for [i (range 100)
        :let [c (count (to-seq seed))]
        :when (> c 100)]
    c))

;; I get a maximum of (167 164546 2369 132 246 665 432 156 616)
