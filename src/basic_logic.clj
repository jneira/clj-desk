(ns basic-logic)

(defn -> [p q] (or (not p) q))
(defn <-> [p q] (and (-> p q) (-> q p)))

(defmacro ? [p]
  (let [exp (if (string? p) (read-string p) p)
        vals [true false]]
    `(for ~['p vals 'q vals :when exp]
       ~{:p 'p  :q 'q}))) 
 
 (assert
 (= (? "(or p (and p (-> q p)))")

    (? (or p (and p (-> q p))))
    
    (for [p [true false] q [true false]
          :when (or p (and p (-> q p)))]
      {:q q, :p p})

    [{:q true, :p true} {:q false, :p true}]))
