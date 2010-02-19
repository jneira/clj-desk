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

(defn sumCarry 
  [acc x]
   (let [s (+ x (acc 0)) [d r] (div s 10)]
     [d (cons r (acc 1))]))

(defn sumListCarry [xs] 
  (let [[x,xs] (reduce sumCarry [0 '()] xs)]
       (if (zero? x) xs (flatten [x,xs]))))

(defn multList [m lst]
     (sumListCarry  (reverse (map #(* %1 m) lst))))
 
(defn multLists [xs ys]
     (let [mults (reduce #(cons (multList %2 xs) %1) '() ys)]
       (sumListCarry (apply map + (multDespl mults)))))

(defn multDespl [lst] 
  (letfn [(pad0 [xs] (map concat xs (iterate #(cons 0 %1) '())))]
	 (pad0 lst)))

(multList 2 '(5 6 7 8))
(multList 1 '(5 6 7 8))
(multLists '(5 6 7 8) '(1 2 3))
(multDespl '((1 7 0 3 4) (1 1 3 5 6) (5 6 7 8)))
(sumListCarry '(12 26 33))
(cons sumListCarry '((8 6) (7 9)))
(apply sumListCarry '((8 6)(7 9)))