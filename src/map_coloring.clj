(ns map-colorize)

(defn colorize [map colors]
  (reduce
   (fn [sols [region neighbours]]
     (for [c colors s sols
           :let [nbcs (for [[nb nbc] s :when (neighbours nb)] nbc)]
           :when (not ((set nbcs) c))]
       (cons [region c] s)))
   [[]] map))

(def map1
  [[1#{2}] [2#{1}]])
(def map2
  [[1 #{2 3 4}] [2 #{1 4 5}]
   [3 #{1 4 6}] [4 #{1,2,3,5,6,7}]  
   [5 #{2 4 7}] [6 #{3 4 7}] [7 #{4 5 6}]])
(def map3 
  [[1 #{2 5 11}] [2 #{1 3 5 11}]
   [3 #{2 4 6 11}] [4 #{3 6 11}]   
   [5 #{1 2 7 11}] [6 #{3 4 8 11}]
   [7 #{5 9 11}] [8 #{6 10 11}]     
   [9 #{7 10 11}] [10 #{9 8 11}]
   [11 #{1 2 3 4 5 6 7 8 9 10}]])

;; (time (dorun (colorize map3 #{:a :b :c :d})))
;; "Elapsed time: 3048.244679 msecs"

(def sort-map
  #(reverse (sort-by (comp count second) %)))

;; (time (dorun (colorize (sort-map map3) #{:a :b :c :d})))
;; "Elapsed time: 221.11679 msecs"
