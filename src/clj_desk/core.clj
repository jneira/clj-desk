(ns clj-desk.core
  (use clojure.walk))

(defn f [x] (* x 2))

(defmacro dup [f]
  (let [[f1 args body] f
        r #(if ((set args) %) `(+ ~% ~%) % )]
    `(~f1 ~args ~(postwalk r body))))
