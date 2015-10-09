(ns crypto.core)

(def english-lwc-freqs
  {\a 8.2 \b 1.5 \c 2.8 \d 4.3 \e 12.7 \f 2.2 \g 2.0 \h 6.1
   \i 7.0 \j 0.2 \k 0.8 \l 4.0 \m 2.4 \n 6.7 \o 1.5 \p 1.9
   \q 0.1 \r 6.0 \s 6.3 \t 9.1 \u 2.8 \v 1.0 \w 2.4 \x 0.2
   \y 2.0 \z 0.1})

(def app-str #(apply str %))
(def hexStrToNum (comp #(Integer/decode %) #(str "0x" %)))
(def hexStrToChar (comp char hexStrToNum))

(defn hexStrToNums [s]
  (->> s (partition 2) (map (comp hexStrToNum app-str))))
(defn hexStrToStr [s]
  (->> s hexStrToNums (map char) app-str))

(defn freqs-nth [s i]
  (->> s hexStrToNums (take-nth i) frequencies))

(def pow2 #(* % %))

(defn sum-distr [freqs]
  (let [c (count freqs)]
    (apply + (map (comp pow2 #(/ % c) second) freqs))))

(defn sums-distr [key-lengths s]
  (for [i key-lengths]
    [i (sum-distr (freqs-nth s i))]))

(defn max-sum-distr [key-lengths s]
  (->> s (sums-distr key-lengths) (sort-by (comp max second)) last))

(def key-length (comp first max-sum-distr))

(defn ith-stream [s n i b]
  (->> s hexStrToNums (drop i) (take-nth n)
       (map (comp char #(bit-xor % b)))))

(defn is-letter? [ch]
  (or (and (> ch 64) (< ch 91)) ; lowercase letters
      (and (> ch 96) (< ch 123)))) ; uppercase letters

(def char-in-range
  #(or (is-letter? % ) ;letters
       (and (> % 47) (< % 58)) ; numbers
       (#{32 33 39 40 41 44 45 46 58 59 63} %))) ; punctuation)

(defn stream-in-range? [s]
  (every? char-in-range (map int s)))

(def is-lwc #(Character/isLowerCase %))

(defn lwc-freqs [s]
  (prn "lwc-freqs")
  (let [lws (filter is-lwc s)
        c (count lws)
        pq #(* (/ (english-lwc-freqs %1) 100) (/ %2 c))]
    (apply + (map #(let [[ch fr] %] (pq ch fr))
                  (frequencies lws)))))

(defn ith-streams [pred s n i]
    (for [b (range 0 256)
        :let [ithcs (ith-stream s n i b)]
        :when (pred n i ithcs)]
      [b ithcs]))

(defn guess-byte [pred s n i]
  (->> (ith-streams pred s n i)
       (sort-by (comp lwc-freqs second))
       last first))

(defn guess-bytes [pred s n]
  (for [i (range 0 n)]
    (guess-byte pred s n i)))

(defn decode [s key]
  (let [keyc (cycle key)
        nums (hexStrToNums s)]
    (app-str (map (comp char bit-xor) nums keyc))))

(defn break
  ([s n] (break #(stream-in-range? %3) s n))
  ([pred s n]
     (let [key (guess-bytes pred s n)]
       (decode s key))))
