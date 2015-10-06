(ns simple-example (:gen-class))
(set! *warn-on-reflection* true)
;; tomado de: http://clojure.org/concurrent_programming
;; usando vectores en lugar de secuencias
(import '(java.util.concurrent Executors Future)
        SimpleLocking$Node)

(defn test-concur [iter refs nthreads niters]
  (let [pool (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                      (fn []
                        (dotimes [n niters]
                          (iter refs t))))
                   (range nthreads))]
    (doseq [^Future future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)))

(defn test-stm [nitems nthreads niters]
  (let [refs (vec (map ref (repeat nitems 0)))
        iter #(dosync (doseq [r %] (alter r + 1 %2)))]
    (test-concur iter refs nthreads niters)
    (map deref refs)))

(defn test-atom [nitems nthreads niters]
  (let [refs (vec (map atom (repeat nitems 0)))
        iter #(doseq [r %] (swap! r + 1 %2))]
    (test-concur iter refs nthreads niters)
    (map deref refs)))

;; SimpleLocking$Node es la clase Node de mi version de java
;; Hasta lo que se yo este codigo en clojure hace lo mismo que el main
(defn test-locking [nitems nthreads niters]
  (let [refs (->> (repeatedly #(SimpleLocking$Node.))
                  (take nitems) vec)
        iter #(doseq [^SimpleLocking$Node n %]
                (.sum n (+ 1 %2)))]
    (test-concur iter refs nthreads niters)
    (map (fn [^SimpleLocking$Node n] (.read n)) refs)))

(definterface INode
  (read [])
  (add [v]))

(deftype Node [^{:unsynchronized-mutable true} value]
  INode
  (read [_] value)
  (add [this v] (set! value (+ value v))))

(defn test-locking-native [nitems nthreads niters]
  (let [refs (->> (repeatedly #(Node. 0))
                  (take nitems) vec)
        iter #(doseq [^Node n %]
                (locking n (.add n (+ 1 %2))))]
    (test-concur iter refs nthreads niters)
    (map (fn [^Node n] (.read n)) refs)))

(defn -main [& args]
  (read-line)
  (let [[type nitems nthreads niters] (map read-string args)
        t #(apply + (time (% nitems nthreads niters)))]
    (case type
          lock (println "Locking:" (t test-locking))
          atom (println "Atom:" (t test-atom))
          stm (println "STM:" (t test-stm))
          lock-native (println "Native locking:" (t test-locking-native)))))
(a v v c)
(-main "lock" "100" "10" "1000")
