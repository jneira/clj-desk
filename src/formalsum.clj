(ns formalsum
  (:require [trammel.provide :as provide]))

(declare sum)

(provide/contracts
 [sum "Dado un numero n devolvera la suma de los numeros entre 1 y n"]
 [n] [number? => (= % (apply + (range 1 n)))])
