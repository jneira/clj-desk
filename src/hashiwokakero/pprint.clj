(ns hashiwokakero.pprint
  (:use (hashiwokakero utilities core)))

(defn str-bridge [panel x y]
  (let [get-val #(or (get-in panel [x %1]) 0)
        [prev val next] (map get-val (list (dec y) y (inc y)))
        horizontal? (or
                     (or (neg? prev) (neg? next))
                     (and (pos? prev) (pos? next)))]
    (if horizontal?
      (if (= -1 val) "-" "=")
      (if (= -1 val) "|" "H"))))

(defn str-cell [panel x y]
  (let [value (get-in panel [x y])]
    (cond-> value
            zero? " "
            neg? (str-bridge panel x y)  
            pos? (str value))))

(defn str-row [panel x]
  (let [row (panel x)
        ymax (count row)]
    (concat '("|")
            (for [y (range 0 ymax)]
              (str-cell panel x y))
            '("|"))))

(defn str-panel [panel]
  (let [xmax (count panel)
        frame-row
        (list (concat '("+")(replicate xmax "-") '("+")))]
    (concat frame-row
            (for [x (range 0 xmax)]
              (str-row panel x))
            frame-row)))

(defn solution-message [num-solutions]
  (case num-solutions
        0 "Puzzle without solution"
        1 "Puzzle with one solution"
        (str "Puzzle with " num-solutions " solutions")))

(defn pprint-panel [panel]
  (dorun (map #(apply println %1) (str-panel panel))))
