(ns alien-calc
  (:require [string-calc :as sc]))


(extend-type java.lang.String
  sc/Adder
  (add [s]
       (.replace s "," "+")))

(extend-type java.util.Collection
  sc/Adder
  (add [s]
       (interleave  s (repeat "+"))))
