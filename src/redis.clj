(ns redis (:require [taoensso.carmine :as car]))
(def pool         (car/make-conn-pool)) ; See docstring for additional options
(def spec-server1 (car/make-conn-spec)) ; ''
(defmacro wcar [& body] `(car/with-conn pool spec-server1 ~@body))
