(ns coursera-fp)

(defn count-change [m [c & cs :as all]]
  (if (or (<= m 0) (empty? all)) 0
      (+ (count-change m cs)
         (if (= m c) 1
             (count-change (- m c) all)))))
