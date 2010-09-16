(ns hashiwokakero.test
  (:use
   (hashiwokakero core pprint utilities)
   (hashiwokakero.test data)))

(def mynode (init-root-node (puzzle 1)))

(def step (first (exploren mynode [0 0 0 0 0 0 0 0 0 0 0 0])))

(do 
  (def p (:panel step))
  (def iss (:islands step))
  (def is (first iss))
  (def c (:coords is))
  (def brs (:bridges is))
  (def br (first brs))
  (pp-panel p))
