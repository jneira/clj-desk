(ns sink)


(def pot #(Math/pow %1 %2))
(defn fact [n]
  (if (zero? n) 1
      (* n (fact (dec n)))))
(def sin #(Math/sin %))
(def pi Math/PI)
(def pi2 (/ pi 2))
(def pi4 (/ pi 4))
(def *2pi (* 2 pi))

(defn tmno [x n] 
  (let [p (Math/pow x n)
        f (fact n)
        r (/ p f)]
    (comment println "|" n p f r) r))

(defn tmno-sink [x n] 
 (comment print x n)
  (let [res (* (Math/pow -1 n)
               (tmno x (+ (* 2 n) 1)))]
    res))

(defn sink-reduce [x k]
  (reduce #(+ %1 (tmno-sink x %2)) x (range 1 (inc k))))

(defn sink [x k]
  (if (zero? k) x
      (+ (tmno-sink x k) (sink x (dec k)))))

(defn isink
  ([x n]
     (let [t (+ (* 2 n) 1)]
       (isink x n
              (* (pot -1 n) (pot x t))
              (fact t))))
  ([x n p f] 
     (let [nxt (/ p f)]
       (comment println x n p f nxt) 
       (if (or (zero? x) (zero? n)) x
           (+ (isink x
                     (dec n)
                     (- (/ p (* x x)))
                      (/ f (* (+ (* 2 n) 1)
                              (* 2 n))))
              nxt)))))

(defn iisink
  ([x n]
     (let [t (+ (* 2 n) 1)
           p (* (pot -1 n) (pot x t))
           f (fact t)]
       (iisink x n p f 0)))
  ([x n p f r]
     (let [nxt (/ p f)]
       (comment println x n p f nxt r) 
       (if (or (zero? x) (zero? n)) (+ x r)
           (recur x
                   (dec n)
                   (- (/ p (* x x)))
                   (/ f (* (+ (* 2 n) 1)
                           (* 2 n)))
                   (+ r  nxt))))))

(defn sinkit [x n]
  (let []
   (with-local-vars [i 0 s x]
     (while (not= @i n)
       (var-set s (+ @s (tmno-sink x (inc @i))))
       (var-set i (inc @i)))
     @s)))
