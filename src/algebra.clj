(ns algebra)

(defn +v [& vs]
  (apply map + vs))

(defn -v [& vs]
     (apply map - vs))
