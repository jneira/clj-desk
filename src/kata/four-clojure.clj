(ns four-clojure)

;;http://4clojure.com/problem/50 -

(def by-type (comp vals (partial group-by type)))

;; amalloy's solution to http://4clojure.com/problem/53
;; 
(let [less (partial apply <)]
   (fn [coll]
     (if-let [pairs
              (seq (->> coll
                        (partition 2 1)
                        (partition-by less)
                        (filter (comp less first))))]
       (apply cons
              ((juxt ffirst
                     (partial map second))
               (apply max-key count (reverse pairs))))
       [])))

;; http://4clojure.com/problem/53
(defn sub [c]
  (let [gt (comp last sort)
        res (gt
            (reduce
             (fn [[x y] n]
               (if (> n (last y))
                 [x (conj y n)]
                 [(gt [x y])
                  (vector n)]))
             [[] [(first c)]] (rest c)))]
    (if (> (count res) 1) res [])))

;; jneira's solution to http://4clojure.com/problem/55

(defn freqs [c]
  (let [x (vals (group-by identity c))]
     (zipmap (map first x) (map count x))))

;; jneira's solution to http://4clojure.com/problem/56
(defn nub [c]
  (reduce
   (fn [acc n]
     (if (some #{n} acc)
       acc
       (conj acc n)))
   [] c))

;; http://4clojure.com/problem/58
(defn componer
  ([f] f)
  ([f1 & fs]
     (fn [& xs]
       (let [res (apply
                  (apply componer fs) xs)]
         (f1 res)))))

;; http://4clojure.com/problem/59
(defn juxt [& fs] (fn [& args] (for [f fs] (apply f args))))

;; jaskriat's solution to http://4clojure.com/problem/59
(fn [& args] (fn [& x] (map #(apply %1 x) args)))

(defn reds
  ([f [h & t]] (reds f h t))
  ([f x [h & t]]
     (let [n (f x h)]
       (if (nil? t) [x n] 
           (cons x (lazy-seq
                    (reds f n t)))))))

;; @chouser  solution to http://4clojure.com/problem/60

(defn reds-it
  ([f coll] (reds-it f (first coll) (rest coll)))
  ([f x coll]
    (->>
      (iterate
        (fn [[x coll :as pair]]
          (when coll
            [(f x (first coll)) (next coll)]))
        [x coll])
      (take-while identity)
      (map first))))

;; jneira's solution to http://4clojure.com/problem/63
(defn group[f c]
  (reduce (fn [m n]
            (merge-with into m {(f n) [n]})) {} c))

;; gshayban's solution to http://4clojure.com/problem/63
(defn group2[f coll]
  (apply merge-with concat (for [x coll] {(f x) (list x)})))

(defn which-coll? [c]
  (let [d [:a 1]
        t1 (conj (empty c) d)
        c? #(contains? %2 %1)]
    (condp c? t1
      :a     :map
      [:a 1] :set
      0      :vector
             :list)))

(defn primes1 [n]
  (letfn
      [(next-prime
        [prs nxt]
        (if (some #(zero? (mod nxt %)) prs)
          (recur prs (inc nxt))
          (conj prs nxt)))]
    (->> (iterate
          (fn [prs] (next-prime prs (inc (last prs)))) [2])
         (take n)
         (last))))

(defn primes2 [n]
  (last
   (take
    n
    (map first
         ((fn iter-when [pred f x]
            (let [iter #(iter-when pred f %)
                  n-cons (f x)]
              (if (pred n-cons x)
                (cons x (lazy-seq (iter n-cons)))
                (iter n-cons))))
          #(not= (first %1) (first %2))
          (fn [[prs nxt :as c]]
            (let [n-prs (if (empty? (filter #(zero? (mod nxt %)) prs))
                          (conj prs nxt) prs)]
              [n-prs (inc nxt)]))
          [[2] 3])))))

(defn primes [n]
  (let
      [next-prime
       (fn [prs]
         (letfn
             [(aux [[f & r :as c] nxt]
                   (cond
                    (empty? c) (conj prs nxt)
                    (zero? (mod nxt f)) (recur prs (inc nxt))
                    :else (recur (take-while #(< % (quot nxt f)) r)
                                 nxt)))]
           (aux prs (inc (last prs)))))]
    (->> (iterate next-prime [2])
         (take n)
         (last))))

(defn primes4 [n]
  (let  [prime?
         (fn [x] (every? #(not= 0 (mod x %)) (range 2 x)))] 
    (take n (filter prime?
                    (drop 2 (range))))))

(defn sieve [n]
  (let [rng (fn [x]
              (iterate #(+ x %) (* x x)))]
    (take
     n
     (cons 2 (lazy-seq
              (iterate
               (fn [cands]
                 [(rest (remove
                         #(some #{%}
                                (rng (first cands)))
                         cands))])
               (filter odd? (drop 3 (range)))))))))

(defn mezcla-con [f h & t]
  (reduce
   (fn [m [k v]] (println m k v (m k))
     (assoc m k (if-let [p (m k)] (f p v) v)))
   h (apply concat t)))

(defn sort-words [str]
  (let [ws (re-seq #"[A-Z|a-z]+" str)]
    (sort-by #(.toLowerCase %) ws)))

(defn tic-tac-toe-winner [board]
  (let [trav (apply map vector board)
        diag #(map-indexed (fn [x y] (y x)) %)  
        all (concat board trav
                    (map diag [board (reverse board)]))]
    (some #(or (:x %) (:o %))
          (filter #(= 1 (count %)) (map set all))) all))

(defn totient [x]
  (let [gcd (fn [a b] 
              (if (zero? b) a,
                  (recur b (mod a b))))
        coprime? (fn [y] (= 1 (gcd x y)))]
    (count (filter coprime? (range 1 (inc x))))))

(defn trampoline [f & xs]
  (let [res (apply f xs)]
    (if (fn? res) (recur res nil)
        res)))

(defn ans [ws]
  (->> (map set ws)
       (zipmap ws)
       (group-by (comp identity second))
       vals
       (map (comp set #(map first %)))
       (filter #(> (count %) 1))
       set))

;; athos's solution to http://4clojure.com/problem/77

(defn ans-great [words]
  (set (for [[_ ws] (group-by sort words)
             :when (> (count ws) 1)]
         (set ws))))

(defn triangle-min-path [tr]
  (first (reduce
          (fn [p n]
            (let [mins (map min p (rest p))]
              (map + mins n)))
          (reverse tr))))

(defn perfect-num? [n]
  (= n
   (apply +
          (filter
           #(zero? (mod n %))
           (range 1 (inc (quot n 2)))))))

(defn to-list- [num]
  (->> num
       (iterate #(quot % 10))
       (take-while #(not= 0%))
       (map #(mod % 10))))

(defn happy-num? [n]
  (let [s (fn [x]
            (->> x to-list-
                 (map #(* % %))
                 (apply +)))]
    (= 1 (some #{1 4} (iterate s n)))))
    
;; jneira's solution to Power Set
;; https://4clojure.com/problem/85
;; translated directly from algorithm described in wikipedia :-P

(defn P [S]
  (if (empty? S) #{S}
      (let [e (first S)
            T (disj S e)
            PT (P T)
            F (fn [e T] (set (map #(conj % e) T)))]
        (into PT (F e PT)))))

;; amalloy's solution to Power Set
;; https://4clojure.com/problem/85

(fn power [coll]
   (if-let [[x & more] (seq coll)]
     (let [p (power more)]
       (into p (for [items p]
                 (conj items x))))
     #{#{}}))

(defn transitive [s]
  (let [f #(for [[a b] % [c d] %
                 :when (= c b)] [a d])]
    (->> s (iterate #(let [n (into % (f %))]
                       (when (not= % n) n)))
         (take-while identity)
         (last))))

;; youz's solution to Transitive Closure
;; https://4clojure.com/problem/84

(fn f [s]
  (#(if (= % s) s (f %))
   (set (for [[a b] s [c d] s]
          [a (if (= b c) d b)]))))


(defn partial-flatten [c]
  (if (coll? c)
    (if (some coll? c)
      (mapcat partial-flatten c) [c])
    c))

(defn read-roman [[f & [s & r :as nxt]]]
  (condp #(or %1 %2) nil
    ({"IV" 4 "IX" 9 "XL" 40 "XC" 90 "CD" 400 "CM" 900}
     (str f s)) :>> #(+ % (read-roman r))
     ({"I" 1 "V" 5 "X" 10 "L" 50 "C" 100 "D" 500 "M" 1000}
      (str f)) :>> #(+ % (read-roman nxt))
      0))

(defn btree? [c]
  (if (coll? c)
    (let [[_ s t] c]
      (and (= (count c) 3)
           (btree? s) (btree? t) ))
    (or (nil? c) (not (not c)))))

(defn btr? [[h s & [t & r :as a]]]
  (let [f #(or (nil? %) (and (coll? %) (btr? %)))]
    (boolean (and (not r) a h (f s) (f t)))))

(defn conn? [c]
  (let [[x & s] (map set c)
        p (group-by #(if (some x %) 1 0) s)
        y (set (apply concat x (p 1)))]
    (or (nil? (p 0))
        (and (not= x y)
             (conn? (cons y (p 0)))))))

(defn conn1? [c]
  (let [[f & r] (seq c)
        h #(apply concat %)]
    (= (set (h c))
     (reduce (fn [a x] 
               (into a (h (filter #(some a %) c))))
             (set f) r))))

(defn symmetric [[_
                  [x l r]
                  [y i d]]]
  (and (= x y)
       (or (= nil l i)
           (and
            (symmetric [_ l d])
            (symmetric [_ r i])))))


;; amalloy's solution to Beauty is Symmetry
;; https://4clojure.com/problem/96

#(= %
    ((fn flip [[v l r :as n]]
       (when n
         [v (flip r) (flip l)]))
     %))

(defn pascal-triangle [n]
  (case n 1 [1]
        (let [p (pascal-triangle (- n 1))
              r (map + p (rest p))]
          (concat [1] r [1]))))


;; Problem 99
(def product-digits
  #(map (comp read-string str) (str (* %1 %2))))

;; Problem 108
(defn lazy-searching [& xs]
  (let [fs (map first xs)]
    (if (apply = fs) (first xs)
        )))

;; Problem 100
(def gcd )

(def lcm)

;; Problem 101

(defn dist [c1 c2]
  (if (and c1 c2 (= c1 c2)) 0 1))

(defn lev-dist [x y]
  (apply + (map dist x y)))

(defn lev-dist [[h & t :as x]
                [i & s :as y]]
  (if (and x y)
    (+ (if (= h i) 0 1)
       (lev-dist t y))
    (max (count x) (count y))))

(comment
  " x 3 2 1 1 2 2

  y 2 1 0 1 2 3
  
  x 1 0 1 2 3 4
    
  # 0 1 2 3 4 5
  # x y y y x"
)

;; Problem 120

(def sum-square-digits 
  #(reduce
    (fn [a c] (if (< c (sum-square c)) (inc a) a))
    0 %))

(defn sum-square [x]
  (apply + (map #(->> % str Integer. ((fn [x] (* x x)))) (str x))))

;; Problem 128

(def card->map
  (fn [[s r]]
    {:suit (case s \D :diamond \H :heart
                 \C :club \S :spades)
     :rank ((zipmap "23456789TJQKA" (range 13)) r)}))

(defn lcm [& xs]
  (ffirst
   (filter #(apply = %)
           (iterate
            #(map-indexed
              (fn [i x] (if (= (apply min %) x)
                          (+ x (nth xs i)) x)) %)
            xs))))

(defn lcm1
  ([x y]
      (letfn [(gcd [x y] (if (= 0 y) x (gcd y (mod x y))))]
        (/ (* x y) (gcd x y))))
  ([x y z & r] (apply lcm1 (lcm1 x y) (lcm1 x z) r)))

;; amcnamara's solution:

(defn lcm2 [a & b]
  (loop [i a]
    (if (every? #(zero? (mod i %)) b)
      i
      (recur (+ i a)))))

;; Problem 147

(defn pascal [x]
  (iterate (fn
             ([[h & s :as a]] 
                  (concat [h] (when s (map + a s)) [(last a)])))
           (map bigint x)))

(defn pascal1 [[h & s :as a]]
  (lazy-cat [a] (pascal1
                 (concat [h]
                         (when s (map #(+ (bigint %) %2) + a s))
                         [(last a)]))))

(defn pascal2
  ([x] [[x] [x x]])
  ([x & y :as a] (let [r (concat [x] (map + a y) [(last a)])]
                   (lazy-cat [r] (pascal2 r)))))

(defn row [side position]
  (let [sq (* side side)]
    (loop [result 0 limit side]
      (if (< position limit)
        result
        (if (< limit sq)
          (recur (inc result) (+ limit side))
          result)))))

(defn row2 [side pos]
  (if (= 1 side) 0
      (quot pos side)))

(defn col [side position]
   (nth (cycle (range side)) position))

(defn col2 [side pos]
   (rem pos side))l
