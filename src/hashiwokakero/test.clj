(ns hashiwokakero.test
  (:use
   (hashiwokakero core pprint utilities)
   (hashiwokakero.test data)))

(def mynode (init-root-node (puzzle 1)))

(do 
  (init-history)
  (def steps (exploren mynode [0 0 0]))
  (dorun (map   pp-node @history))
  (println "steps" (count steps)))

(do

  (def step (first steps))
  (def p (:panel step))
  (def iss (:islands step))
  (def is (first iss))
  (def c (:coords is))
  (def brs (:bridges is))
  (def br (first brs))
  (println "STEPS ************************")
  (dorun (map pp-node steps)))

;; (time (dorun (map resolve-puzzle (take 40 puzzle))))
;; "Elapsed time: 78206.390756 msecs" 1.3034398459333332 minutos
;; (time (dorun (map resolve-puzzle (subvec puzzle 40 50))))
;; "Elapsed time: 146647.332203 msecs" 2.4441222033833334 minutos
