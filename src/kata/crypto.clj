(ns crypto)

(defn caesar [n s]
  (apply str
         (for [c s]
           (if (Character/isLetter c)
             (char (+ n (int c))) c))))

(def upper-case #(.toUpperCase (str %)))

(defn vigenere [m c]
  (let [g #(int (upper-case %))
        f #(char (+ 65 (mod (+ (g %) (g %2) -130) 26)))]
    (apply str (map f m (cycle c)))))



