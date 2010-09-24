(ns hashiwokakero.utilities)

(defn transpose [matrix]
  (when matrix (apply map vector matrix))) 
(defn abs [n] (when n (Math/abs n)))
(defn sum [seq] (apply + seq))
(def º comp)
(def ¬ partial)

(defn no-empty-seq? [arg]
  (and (seq? arg)) ((comp not empty?) arg))

(defn backtracking [solution? explore node]
   (if (solution? node) [node]
      (mapcat
       #(backtracking solution? explore %1)
       (explore node))))

(defmacro cond-> [arg & clauses]
  (let [vclauses (vec clauses)
        clauses-> 
        (for [i (range 0 (count vclauses))
              :let [clause (vclauses i)]]
          (if (even? i) (concat `(-> ~arg)
                              (list clause))
              clause))] 
    (cons `cond clauses->)))

