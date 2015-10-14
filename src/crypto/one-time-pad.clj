(ns crypto.one-time-pad
   (:require [crypto.core :as crypto]))

(def cypher-texts
  ["BB3A65F6F0034FA957F6A767699CE7FABA855AFB4F2B520AEAD612944A801E",
   "BA7F24F2A35357A05CB8A16762C5A6AAAC924AE6447F0608A3D11388569A1E",
   "A67261BBB30651BA5CF6BA297ED0E7B4E9894AA95E300247F0C0028F409A1E",
   "A57261F5F0004BA74CF4AA2979D9A6B7AC854DA95E305203EC8515954C9D0F",
   "BB3A70F3B91D48E84DF0AB702ECFEEB5BC8C5DA94C301E0BECD241954C831E",
   "A6726DE8F01A50E849EDBC6C7C9CF2B2A88E19FD423E0647ECCB04DD4C9D1E",
   "BC7570BBBF1D46E85AF9AA6C7A9CEFA9E9825CFD5E3A0047F7CD009305A71E"])

(def known-key-length 31)

(def cypher-text (apply str cypher-texts))

(defn xor-all [ss]
  (letfn [(rec [[n & ns]]
            (when ns
              (concat [(for [m ns] (map bit-xor n m))]
                      (rec ns))))]
    (rec  (map crypto/hexStrToNums ss))))

(defn is-xor-with-spc? [n]
  (and (>= n 64) (< n 128)))

(defn idxs-with-spc [ns]
  (remove nil?
          (map-indexed #(when (is-xor-with-spc? %2)  %1) ns)))

(defn all-idxs-with-spc [xa]
  (let [count-spc
        (fn [i] (->> xa (map (comp #(some #{i} %) idxs-with-spc))
                    (remove nil?) count))]
    (filter #(> (count-spc %) 1) (range 0 known-key-length))))

(defn get-chars [xa idx]
  (cons \space (map #(char (bit-xor % 32))
                    (map #(nth % idx) xa))))

(defn is-lwc-or-spc [ch]
  (or (crypto/is-letter? (int ch)) (= 32 (int ch))))

(defn guess-chars [nss]
  (let [xas (xor-all nss)
        idxss (map all-idxs-with-spc xas)]
    (into {}
          (map-indexed
           (fn [i idxs]
             (let [xa (nth xas i)]
               [i (into {}
                        (for [idx idxs
                              :let [chrs (vec (get-chars xa idx))]
                              :when (every? is-lwc-or-spc chrs)]
                          [idx chrs]))])) idxss))))
(defn get-guessed-chars [chrs i j]
  (set (for [k (range 0 (count chrs))
             :let [di (- i k)
                   ch (get-in chrs [k j di])]
             :while (>= di 0)
             :when ch] ch)))

(defn in-range? [agchs j i ch]
  (let [gchs (get-guessed-chars agchs i j)
        in (and (is-lwc-or-spc ch)
                (or (empty? gchs) (gchs ch)))]
    in))

(defn stream-in-range? [agchs j ithcs]
  (when (= 1 j) (println ithcs))
  (every? identity  (map-indexed (partial in-range? agchs j) ithcs)))

(defn solution
  ([] (solution cypher-texts))
  ([cts]
      (let [agchs (guess-chars cts)]
        (crypto/break #(stream-in-range? agchs %2 %3)
                      (apply str cts) known-key-length))))

;; follow
