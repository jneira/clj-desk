(ns hashiwokakero.test
  (:use
   (hashiwokakero core pprint)
   (hashiwokakero.test data)))

(def mynode (init-root-node (puzzle 1)))

(def p (:panel mynode))
(def iss (:islands mynode))
(def is (first iss))
(def c (:coords is))
(def brs (:bridges is))
