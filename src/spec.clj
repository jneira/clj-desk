(ns spec
  (:require (trammel (core :as tr)))
  (:use (clojure.contrib
         (seq :only (positions)))))

;; La division entera (pg 47)

(tr/defcontract int-div-bad-contract
  "El contrato reproduce la defincion matematica pero
    es incorrecto."
  [a b] [(every? integer? [a b])
         (not= 0 b)
         =>
         (every? integer? %)
         (let [q (first %) r (second %)]
           (= a (+ (* b q) r)))])

(def bad-impl-int-div
     (tr/with-constraints
       (fn [a b] [0 0])
       int-div-bad-contract))

(def lazy-impl-int-div
     (tr/with-constraints
       (fn [a b] [0 a])
       int-div-bad-contract))

;(bad-impl-int-div 10 4)

;(lazy-impl-int-div 10 4)

(tr/defcontract int-div-contract
  "El contrato reproduce la definicion matematica 
    de division entera."
  [a b] [(every? integer? [a b])
         (not= 0 b)
         =>
         (every? integer? %)
         (let [q (first %) r (second %)]
           (= a (+ (* b q) r))
           (<= 0 r) (< r b))])

(def lazy-impl-int-div
     (tr/with-constraints
       (fn [a b] [0 a])
       int-div-contract))

;(lazy-impl-int-div 10 4)

(def int-div
     (tr/with-constraints
       (fn [a b] [(quot a b) (rem a b)])
       int-div-contract))

;; Calculo del maximo de un vector

(tr/defcontract bad-max-vector-contract
  "Especificacion incorrecta del calculo del
   maximo de un vector de enteros"
  [a] [(vector? a) (every? integer? a)
         =>
         (integer? %)
         (every? (fn [x] (>= % x)) a)])

(def lazy-max-vector-impl
     (tr/with-constraints
       (fn [a] (Integer/MAX_VALUE))
       bad-max-vector-contract))

(lazy-max-vector-impl [1 2 1 3])

(tr/defcontract max-vector-contract
  "Especificacion del calculo del maximo de un vector de enteros"
  [a] [(vector? a) (every? integer? a)
       =>
       (integer? %)
       (every? (fn [x] (>= % x)) a)
       (some (fn [x] (= % x)) a)])

(def lazy-max-vector-impl
     (tr/with-constraints
       (fn [a] (Integer/MAX_VALUE))
       max-vector-contract))

;(lazy-max-vector-impl [1 2 1 3] 3)

(def max-vector-impl
 (tr/with-constraints
   (fn [a] (apply max a))
   max-vector-contract))

(max-vector-impl [1 2 1 3])

;; Funcion que calcula una posicion cualquiera de uno de los
;; posibles maximos de un vector

(tr/defcontract una-posicion-max-vector-contract
  "Especificacion del calculo de una posicion cualquiera entre los maximos de un vector"
  [a] [(vector? a) (every? integer? a)
       =>
       (integer? %)
       (>= % 1) (< % (count a))
       (every? (fn [x] (>= (a %) x)) a)])

(defn rand-pos-max [a]
  (let [max (apply max a)]
    (rand-nth (positions #(= max %) a))))

(def restricted-pos-max
     (tr/with-constraints
       rand-pos-max una-posicion-max-vector-contract))

;; Funcion que devuelve la posicion del primero de los maximos

(tr/defcontract primera-posicion-max-vector-contract
  "Especificacion del calculo de la primera posicion  entre los maximos de un vector"
  [a] [(vector? a) (every? integer? a)
       =>
       (integer? %)
       (>= % 1) (< % (count a))
       (every? (partial >= (a %)) a)
       (every? (partial > (a %))
               (take % a))])

(def restricted-bad-first-pos-max
     (tr/with-constraints
       rand-pos-max primera-posicion-max-vector-contract))

(defn first-pos-max [a]
  (let [max (apply max a)]
    (first (positions #(= max %) a))))

(def restricted-first-pos-max
     (tr/with-constraints
       first-pos-max primera-posicion-max-vector-contract))

;; Mutacion del parametro, sustituir elementos

(declare *prev*)

(tr/defcontract sustituir-vector-contract
  "Sustitucion de los elementos de un vector"
  [a x y]
  [(instance? clojure.lang.ARef a)
   (vector? @a)
   (every? integer? [x y]) 
   =>
   (vector? @a)
   (every?  true?
           (map #(or (and (= %1 x)
                          (= %2 y))
                     (= %1 %2))
                *prev* @a))])

(defn sustituir [a x y]
  (swap! a (constantly
            (vec (map #(if (= x %) y %) @a))))
  @a)

(def *prev* [])
(def v (atom [1 0 2 0]))

(def sustituir-c
     (tr/with-constraints
       (fn  [a x y]
         (binding [*prev* []]
           (set! *prev* a) (sustituir a x y)))
       sustituir-vector-contract))

(sustituir v 0 -1)

(defn frequency [coll e]
  ((frequencies coll) e))

(tr/defcontract moda-contract
  "El elemento que mas veces aparece en una coleccion"
  [a] [(vector? a)
       =>
       (let [freq (fn [x]
                    (frequency a x))]
         (every? (fn [x] (>= (freq %) (freq x))) a))])

(defn moda [coll]
  (first (last (sort-by second (frequencies coll)))))

(def moda-c
     (tr/with-constraints
       moda moda-contract))


;; Problema 2.1

;; frec (y,a,n)=(N β ∈ {1..n}.(a[β] = y)
;; perm (a,b,n) = (∀α ∈ {1..n}.(∃β ∈ {1..n}.(a[α]=b[β]) ∧ (frec(a[α],a,n)=frec(b[β],b,n))

(defcontract permutations?-contract
  "Contrato que define el predicado "
  [& cs] )

(defn permutations? [& cs]
  (reduce =  (map #(sort (frequencies %)) cs))) 

