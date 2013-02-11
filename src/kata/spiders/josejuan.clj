(ns josejuan-spiders)

; size, grid y arañas 

(def n 6) 
(def g (vec (map ref (repeat (* n n) 0)))) 
(def s (vec (map ref (map #(do [%1 %1]) (range 0 n))))) 

; incrementa (shared) un nodo 
(defn i[n] (dosync (alter (g n) + 1))) 
(defn ii[[x y]] (i (+ x (* y n))))

; mueve una araña (shared) en un toroide 

(defn am[p] (vec (map #(mod (+ n %1) n)
                      (map + p (rand-nth [[0 1] [1 0] [0 -1] [-1 0]]))))) 
(defn aam[z] (dosync (alter (s z) am))) 

; spider loop 
(defn sp[z p] (dotimes [r p] (aam z) (ii @(s z)))) 

; ejecuta en paralelo el movimiento de las arañas, cada araña da 'p' pasos independientemente de las otras 

(defn spiders[p]
  (dorun (apply pcalls (map (fn [z] (fn [] (sp z p)))
                            (range 0 n))))) 

(defn run [n]
  (time (spiders n))
  (apply + (map deref g)))
