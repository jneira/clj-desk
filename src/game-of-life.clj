(ns juego-de-la-vida
  (:use clojure.test))

(defn esta-viva [celula]
  (= :viva (:estado celula)))

(defn esta-muerta [celula]
  (= :muerta (:estado celula)))

(defn evoluciona [celula]
  (cond  (and (esta-viva celula)
              (or  (< (:num-vecinos celula) 2)
                   (> (:num-vecinos celula) 3)))
         (assoc celula :estado :muerta)
         (and (esta-muerta celula)
              (= 3 (:num-vecinos celula)))
         (assoc celula :estado :viva)
         :else celula))

(defstruct Celula :estado :num-vecinos)

(deftest una-celula-viva-con-menos-de-dos-vecinos-no-vive
  (let [celula (struct Celula :viva 1)
        next-celula (evoluciona celula)]
    (is (= :muerta (:estado next-celula)))))

(deftest una-celula-viva-con-dos-o-tres-vecinos-sigue-viva
  (let [celula (struct Celula :viva 2)
        next-celula (evoluciona celula)]
    (is (= :viva (:estado next-celula)))))

(deftest una-celula-viva-con-mas-de-tres-vecinos-no-vive
  (let [celula (struct Celula :viva 4)
        next-celula (evoluciona celula)]
    (is (= :muerta (:estado next-celula)))))

(deftest una-celula-muerta-con-tres-vecinos-revive
  (let [celula (struct Celula :muerta 3)
        next-celula (evoluciona celula)]
    (is (= :viva (:estado next-celula)))))

(run-tests)
