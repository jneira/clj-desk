(ns auto-numbers)

(set! *warn-on-reflection* true)

(defn digitToInt [c] (- (int c) 48)) 

(defn d [n] (apply + n (map digitToInt (str n))))

(defn noSelf [v i n] 
  (if (<= i n)
        (do (aset-boolean v i true) 
            (recur v (d i) n)))) 

(defn selfs [n] 
    (let [v (boolean-array (+ 1 n))] 
         (for [i (range 1 (+ 1 n)) 
               :when (do (noSelf v (d i) n)
                         (not (aget v i)))] i)))

(defn upTo [n] (reverse (take 20 (reverse (sort (selfs n)))))) 

(defn test []  (print (with-out-str (time (println (upTo 9999)))))) 
