(ns greed)

(defn score-single [ps]
  (case ps 1 100 5 50 0))

(defn score-triple [ps]
  (case ps 1 1000 (* ps 100)))

(defn scores-by-freq [[ps freq]]
  [(* (quot freq 3)
      (score-triple ps))
   (* (mod freq 3)
      (score-single ps))])

(defn score [dice-roll]
  (->> (frequencies dice-roll)
      (mapcat scores-by-freq)
      ( apply +)))

