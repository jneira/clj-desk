(ns hashiwokakero.test
  (:use
   (hashiwokakero core pprint)
   (hashiwokakero.test data)))

(def mynode (init-root-node (puzzle 1)))

(def p (:panel mynode))
(def iss (:islands mynode))
(def is (last iss))
(def c {:x 4 :y 12})
(def brs (:bridges is))
