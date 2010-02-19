(ns mult-listas)

(defn div [x y] [(quot x y) (rem x y)])

(defn numList [x]
  (loop [acc () sig x]
    (let [[cociente resto] (div sig 10) lista (cons resto acc)]
      (if (zero? cociente) lista (recur lista cociente)))))

(defn listNum [lst] (reduce #(+ %2 (* 10 %1)) 0 lst))

(assert (= (list 1 2 3 4 5) (numList 12345)))
(assert (= 12345 (listNum '(1 2 3 4 5))))
