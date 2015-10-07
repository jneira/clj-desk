(ns crypto.one-time-pad
   (:use [crypto.core]))

(def cypher-texts
  ["BB3A65F6F0034FA957F6A767699CE7FABA855AFB4F2B520AEAD612944A801E",
   "BA7F24F2A35357A05CB8A16762C5A6AAAC924AE6447F0608A3D11388569A1E",
   "A67261BBB30651BA5CF6BA297ED0E7B4E9894AA95E300247F0C0028F409A1E",
   "A57261F5F0004BA74CF4AA2979D9A6B7AC854DA95E305203EC8515954C9D0F",
   "BB3A70F3B91D48E84DF0AB702ECFEEB5BC8C5DA94C301E0BECD241954C831E",
   "A6726DE8F01A50E849EDBC6C7C9CF2B2A88E19FD423E0647ECCB04DD4C9D1E",
   "BC7570BBBF1D46E85AF9AA6C7A9CEFA9E9825CFD5E3A0047F7CD009305A71E"])

(def cypher-text (apply str cypher-texts))

(defn xor-all [ss]
  (letfn [(rec [[n & ns]]
            (when ns
              (concat [(for [m ns] (map bit-xor n m))]
                      (rec ns))))]
    (rec  (map hexStrToNums ss))))

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
  (map #(char (bit-xor % 32))
       (map #(nth % idx) xa)))

(defn is-lwc-or-spc [ch]
  (or (is-letter? (int ch)) (= 32 (int ch))))

(defn guess-chars [nss]
  (let [xas (xor-all nss)
        idxss (map all-idxs-with-spc xas)]
    (for [idxs idxss i (range 0 (count idxss))
          :let [xa (nth xas i)]]
      [i (for [idx idxs
               :let [chrs (get-chars xa idx)]
               :when (every? is-lwc-or-spc chrs)]
           [idx chrs])])))

(def known-key-length 31)
