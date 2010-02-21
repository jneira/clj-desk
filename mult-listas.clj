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

(defn carry 
  [acc x] (let [s (+ x (acc 0)) 
		[d r] (div s 10)]
	    [d (cons r (acc 1))]))

(defn listCarry [xs] 
  (let [[x,xs] (reduce carry [0 []] (reverse xs))]
       (if (zero? x) xs (cons x xs))))

(assert (= [1 '(5)] (carry [0 ()] 15)))
(assert (= [1 '(4 5)] (carry [1 '(5)] 13)))
(assert (= [1 '(2 4 5)] (carry [1 '(4 5)] 11)))
(assert (= '(1 2 4 5) (listCarry '(11 13 15))))

(defn lpad [xs x y] (concat (replicate x y) xs)) 
(defn maxLength [& lst] (apply max (map count lst)))

(assert (= '(0 0 1 2 3 4) (lpad '(1 2 3 4) 2 0)))
(assert (= 5 (maxLength '(1 2 3) '(1 2) '(1 2 3 4 5) '(1))))
 
(defn multDespl [lst] 
  (letfn [(pad0 [xs] (map concat xs 
		     (iterate #(cons 0 %1) '())))]
	 (pad0 lst)))

(assert (= '((1 2 3) (1 2 0) (1 0 0)) (multDespl '((1 2 3) (1 2) (1)))))

(defn multList [m lst]
     (listCarry (map #(* %1 m) lst)))

(defn multLists [xs ys]
     (let [mults (reduce #(cons (multList %2 xs) %1) '() ys)
	   despl (multDespl mults)
	   max (apply maxLength despl)
	   lpaded (map #(lpad %1 (- max (count %1)) 0) despl)]
       (listCarry (apply map + lpaded))))

(assert (= 11356 (listNum (multList 2 '(5 6 7 8)))))
(assert (= 698394 (listNum (multLists '(5 6 7 8) '(1 2 3)))))
