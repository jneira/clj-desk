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
  (let [[x,xs] (reduce sumCarry [0 '()] (reverse xs))]
       (if (zero? x) xs (flatten [x,xs]))))

(defn multList [m lst]
     (sumListCarry (map #(* %1 m) lst)))
 
(defn multLists [xs ys]
     (let [mults (multDespl 
		  (reduce #(cons (multList %2 xs) %1) '() ys))
	   max (apply maxLength mults)
	   lpaded (map #(lpad %1 (- max (count %1)) 0) mults)]
       (sumListCarry (apply map + lpaded))))

(defn multDespl [lst] 
  (letfn [(pad0 [xs] (map concat xs 
		     (iterate #(cons 0 %1) '())))]
	 (pad0 lst)))

(defn lpad [xs x y] (concat (replicate x y) xs)) 

(defn maxLength [& rest] (apply max (map count rest)))


(assert (= 11356 (listNum (multList 2 '(5 6 7 8)))))
(assert (= 698394 (listNum (multLists '(5 6 7 8) '(1 2 3)))))
(assert (= [2 '(8)] (sumCarry [5 []] 23)))
(assert (= (1 4 7 3) (sumListCarry '(12 26 13))
(multDespl '((1 7 0 3 4) (1 1 3 5 6) (5 6 7 8)))
(lpad '(1 2 3) 2 0)
(maxLength '(1 2 3) '(1 2 3) '(2 3 4))