(ns crypto.vigenere)

(def cypher-text "F96DE8C227A259C87EE1DA2AED57C93FE5DA36ED4EC87EF2C63AAE5B9A7EFFD673BE4ACF7BE8923CAB1ECE7AF2DA3DA44FCF7AE29235A24C963FF0DF3CA3599A70E5DA36BF1ECE77F8DC34BE129A6CF4D126BF5B9A7CFEDF3EB850D37CF0C63AA2509A76FF9227A55B9A6FE3D720A850D97AB1DD35ED5FCE6BF0D138A84CC931B1F121B44ECE70F6C032BD56C33FF9D320ED5CDF7AFF9226BE5BDE3FF7DD21ED56CF71F5C036A94D963FF8D473A351CE3FE5DA3CB84DDB71F5C17FED51DC3FE8D732BF4D963FF3C727ED4AC87EF5DB27A451D47EFD9230BF47CA6BFEC12ABE4ADF72E29224A84CDF3FF5D720A459D47AF59232A35A9A7AE7D33FB85FCE7AF5923AA31EDB3FF7D33ABF52C33FF0D673A551D93FFCD33DA35BC831B1F43CBF1EDF67F0DF23A15B963FE5DA36ED68D378F4DC36BF5B9A7AFFD121B44ECE76FEDC73BE5DD27AFCD773BA5FC93FE5DA3CB859D26BB1C63CED5CDF3FE2D730B84CDF3FF7DD21ED5ADF7CF0D636BE1EDB79E5D721ED57CE3FE6D320ED57D469F4DC27A85A963FF3C727ED49DF3FFFDD24ED55D470E69E73AC50DE3FE5DA3ABE1EDF67F4C030A44DDF3FF5D73EA250C96BE3D327A84D963FE5DA32B91ED36BB1D132A31ED87AB1D021A255DF71B1C436BF479A7AF0C13AA14794")

(def key-lengths (range 1 14))
(def app-str #(apply str %))

(def english-lwc-freqs
  {\a 8.2 \b 1.5 \c 2.8 \d 4.3 \e 12.7 \f 2.2 \g 2.0 \h 6.1
   \i 7.0 \j 0.2 \k 0.8 \l 4.0 \m 2.4 \n 6.7 \o 1.5 \p 1.9
   \q 0.1 \r 6.0 \s 6.3 \t 9.1 \u 2.8 \v 1.0 \w 2.4 \x 0.2
   \y 2.0 \z 0.1})

(def hexStrToNum (comp #(Integer/decode %) #(str "0x" %)))
(def hexStrToChar (comp char hexStrToNum))

(defn hexStrToNums [s]
  (->> s (partition 2) (map (comp hexStrToNum app-str))))
(defn hexStrToStr [s]
  (->> s hexStrToNums (map char) app-str))

(defn freqs-nth [s i]
  (frequencies (take-nth i (hexStrToNums s))))

(def pow2 #(* % %))

(defn sum-distr [freqs]
  (let [c (count freqs)]
    (apply +  (map (comp pow2 #(/ % c) second) freqs))))

(defn sums-distr [s]
  (for [i key-lengths]
                   [i (sum-distr (freqs-nth s i))]))

(defn max-sum-distr [s]
  (last (sort-by (comp max second)
                 (sums-distr s))))

(def key-length (comp first max-sum-distr))

(defn ith-cypher-stream [s n i b]
  (map (comp char #(bit-xor % b))
       (take-nth n  (drop i (hexStrToNums s)))))

(defn in-range [s]
  (every? #(and (> % 31) (< % 128)) (map int s)))

(def is-lwc  #(Character/isLowerCase %))

(defn lwc-freqs [s]
  (let [lws (filter is-lwc s)
        c (count lws)
        pq #(* (/ (english-lwc-freqs %1) 100) (/ %2 c))]
    (map  #(let [[ch fr] %] [ch (pq ch fr)]) (frequencies lws))))

(defn max-lwc-freqs [ss]
  (sort-by ))

(defn guess-byte [s n i]
  (map (comp app-str second)
       (filter (comp in-range second)
               (for [b (range 0 256)]
                 [b (ith-cypher-stream s n i b)]))))

