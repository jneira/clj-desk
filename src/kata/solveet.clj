(ns kata.solveet
  (use clojure.repl clojure.math.combinatorics clojure.java.io))

(defn arbol [n]
  (doseq [x (range 1 (+ n 1))]
    (apply println (mapcat repeat [(- n x) (- (* 2 x) 1)] [" " "*"]))))

(def bosque
  (iterate #(concat (map (fn [x] (cons " " x)) %)
                    [(repeat (+ 1 (* 2 (count %))) \*)])
           ["*"]))

(def digit #(nth (mapcat str (range)) %)) 

(defn caracol [n])

(defn rle [s]
  (->> s
       (partition-by identity)
       (mapcat #(partition-all 5 %))
       (mapcat #(cons (count %) (take 1 %)))
       (apply str)))

(defn euclidean-distance [p q]
  (Math/sqrt (apply + (map (comp #(* % %)  -) p q))))

;; http://www.solveet.com/exercises/El-numero-feliz/73
(defn to-list [num] 
  (->> num 
       (iterate #(quot % 10)) 
       (take-while #(not= 0%)) 
       (map #(mod % 10)))) 

(defn happy-num? [n] 
  (let [s (fn [x] 
            (->> x to-list 
                 (map #(* % %)) 
                 (apply +)))] 
    (= 1 (some #{1 4} (iterate s n))))) 

;; Numero perfecto http://www.solveet.com/exercises/Numero-perfecto/76
(defn divisors [n]
  (filter #(= 0 (rem n %)) (range 1 (+ 1 (quot n 2)))))

(defn perfect-num? [n]
  (= n (apply + (divisors n))))

(defn perfect-nums
  [n] (filter perfect-num? (range n)))

;; 
(defn happy-num? [n] (println n)
  (or (= n 1) (and (not= n 4)
                   (->> n str
                        (map #(Character/digit % 10))
                        (map #(* % %))
                        (apply +)))))

(defn f [x y z]
  (if (<= x y) y
      (f (f (- x 1) y z)
         (f (- y 1) z x)
         (f (- z 1) x y))))

(defn f2 [x y z]
  (iterate
   (fn [[[x y z] & xs]]
     (concat [[(- x 1) y z]
              [(- y 1) z x]
              [(- z 1) x y]] xs))
   [[x y z]]))
(defn f3 [x y z]
  ((comp second first)
   (mapcat #(filter
             (fn [[x y z]] (<= x y)) %)
           (f2 x y z))))

(defn testf [n] (f n 0 (+ n 1)))
(defn testf3 [n] (f3 n 0 (+ n 1)))

;; user> (time (testf 11))
;; "Elapsed time: 12194.863942 msecs"
;; 12

;; user> (time (testf2 11))
;; "Elapsed time: 0.226775 msecs"
;; 12

;; user> (time (testf2 600000))
;; "Elapsed time: 0.218673 msecs"
;; 600001


;; user> (time (testf3
;; 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000))
;; "Elapsed time: 1.134642 msecs"
;; 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a5M000000000000000000000000000000000000000000000000000000000000000000000000000000001N

;; http://www.solveet.com/exercises/Elegancia-funcion-conversion-de-tipo-numerico/93
(defn ubyte [n] (bit-and 0xFF n))

(def tree [2
           [7 3
            [6 5 11]]
           [8 nil
            [9 4 nil]]])

(def pre-order [2 7 3 6 5 11 8 9 4])
(def in-order [3 7 5 6 11 2 8 9 4])

(def porst-order [3 5 11 6 7 4 9 8 2])

(defn post-walk [[root & pre] in]
  (when root
    (let [[lin [_ & rin]]
          (split-with (comp not #{root}) in)
          [lpre rpre]
          (split-with (set lin) pre)]
        (concat (post-walk lpre lin)
                (post-walk rpre rin)
                [root]))))

(defn probability [n]
  (- 1.0 (/ 1 (.pow (biginteger 2) n))))

;; http://docs.oracle.com/javase/6/docs/api/java/math/BigInteger.html#isProbablePrime%28int%29
(defn mersenne [prob]
  (->> (drop 2 (range))
       (map #(.subtract
              (.pow (biginteger 2) %)
              (biginteger 1)))
       (filter #(.isProbablePrime % prob))))

(def data "1,pepe,2012;4,juan,2024;2,andres,2009")

(defn str-map [s f c]
  (apply hash-map
         (mapcat
          #(let [[k & vs] (.split % c)] [k vs])
          (.split s f))))

(defn str-map2 [s f c]
  (apply hash-map
         (apply concat
                (for [row (.split s f)
                      :let [[key & vals] (.split row c)]]
                  [key vals]))))

(defn str-map3 [s f c]
  (reduce (fn [m [key & vals]]
            (assoc m key vals)) {}
            (map #(.split % c) (.split s f))))

(defn str-map4 [s f c]
  (reduce
   #(let [[k & vs] (.split %2 c)]
      (assoc % k vs)) {}
      (.split s f)))

(defn str-map5 [s f c]
  (apply merge
         (for [row (.split s f)
               :let [[key & vals] (.split row c)]]
           (hash-map  key vals))))

(def abc (map (comp list char) (range 65 91)))

(defn ordered-combinations [seed]
  (let [seed (map list seed)
        next #(for [x %] (for [y seed] (concat x y)))]
    (->> (iterate #(apply concat (next %)) seed)
         (mapcat identity))))

(def cols
  (let [abc (map char (range 65 91))]
    (map #(apply str %) (ordered-combinations abc))))

(defn =leaves [t1 t2]
  (let [leaves #(remove (fn [x] (or (coll? x) (nil? x)))
                        (tree-seq coll? rest %))]
    (apply =  (map leaves [t1 t2]))))

(defn make-tree [x] 
  (iterate #(do [x % %]) [x x x]))

(defn tree-proof [n]
 (last (take n (make-tree 5))))

(defn =leaves-proof [n]
  (let [t (tree-proof n)]
    (time (=leaves t t))))

;; (=leaves-proof 15)
;; "Elapsed time: 996.440958 msecs"
;; true

(defn list-dirs [path]
  (let [ds (filter #(.isDirectory %) (.listFiles (file path)))]
    (println (map str ds) (count ds))))

(def dir-filter
  (reify java.io.FileFilter (accept [t f] (.isDirectory f))))

(defn list-dirs2 [path]
  (let [ds (.listFiles (file path) dir-filter)]
    (println (map str ds) (count ds))))

;; http://www.solveet.com/exercises/Generador-de-arboles-binarios/92
0
(defn randtree [max prob]
  (let [f (fn [[pre in post] lvl]
            (when (and (not= lvl prob )
                       (<= (rand) prob))
              ))]
    (iterate f '([[1] [1] [1]] 0))))

;; [1] [1] [1]

;; [1 2]
;; [2 1] [2 1] // [1 2] [2 1]

;; [1 2 3]
;; [2 1 3] [2 3 1] // [3 2 1] [3 2 1]
;; [2 3 1] [3 2 1] //

;; [1 2 3 4]
;; [4 2 1 3] [4 2 3 1] // [2 4 1 3] [4 2 3 1]
;; [2 1 4 3] [2 4 3 1] // [2 1 3 4] [2 4 3 1]

;; http://http://www.solveet.com/exercises/Todas-las-combinaciones-ordenadas-de-un-conjunto/111

(defn combs [seed]
    (->> (map list seed)
         (iterate #(for [x % y seed] (concat x [y])))
         (mapcat identity)))
