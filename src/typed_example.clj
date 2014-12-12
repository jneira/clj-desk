(ns typed-example
  (:require [clojure.core.typed :as typed]
            [clojure.core.typed.type-ctors :as tc]))


(typed/def-alias Color (U (Value :red) (Value :green) (Value :blue)))

(declare c)
(typed/ann c Color)

(def c 1)



