(ns mult-listas
  (:use clojure.contrib.seq-utils)))

(defn div [x y] [(quot x y) (rem x y)])

(defn numList [x]
  (loop [acc () sig x]
    (let [[cociente resto] (div sig 10) lista (cons resto acc)]
      (if (zero? cociente) lista (recur lista cociente)))))

(defn listNum [lst] (reduce #(+ %2 (* 10 %1)) 0 lst))

(assert (= (list 1 2 3 4 5) (numList 12345)))
(assert (= 12345 (listNum '(1 2 3 4 5))))

(defn sumListAc [acc x]
   (let [s (+ x (acc 0)) [d r] (div s 10)]
     [d (cons r (acc 1))]))
(defn multList [m lst]
     (let [[x,xs] (reduce sumListAc [0 '()]
                              (reverse (map #(* %1 m) lst)))]
      (if (zero? x) xs (flatten [x,xs]))))
(defn multLists [xs ys]
     (reduce #(cons (multList %2 '(4 5 6)) %1) '() '(4 5)))

(multList 2 '(5 6 7 8))
(multLists '(5 6 7 8) '(1 2))
