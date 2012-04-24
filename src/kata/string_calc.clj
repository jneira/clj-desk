(ns kata.string-calc)

(defprotocol Adder
  (add [self]))

(extend-type java.lang.String
  Adder
  (add [s]
       (apply + (map #(Integer/parseInt %)
                     (.split s ",")))))

(extend-type java.util.Collection
  Adder
  (add [s]
       (apply +  s)))

(extend-type java.util.Map
  Adder
  (add [s]
       (apply + (vals s))))

;;string-calc> (add (doto (new java.util.ArrayList) (.add  1) (.add 2)))
;;3
;;string-calc> (add (doto (new java.util.Vector) (.add  1) (.add 2)))
;;3
;;string-calc> (add "3,4")
;;7
;;string-calc> (add (doto (java.util.HashMap.) (.put "one" 1) (.put "two" 2)))
;;3
