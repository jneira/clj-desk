(ns string-calc)

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

;;string-calc> (add (doto (new java.util.ArrayList) (.add  1) (.add 2)))
;;3
;;string-calc> (add (doto (new java.util.Vector) (.add  1) (.add 2)))
;;3
;;string-calc> (add "3,4")
;;7
