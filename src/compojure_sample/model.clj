(ns compojure-sample.model)

(def heroes
  (atom {}))
	 
(defn get-facts [hero] 
  (get @heroes hero))

(defn add-fact! [hero fact]
	(swap! heroes 
               (fn [h]
                 (let [facts (get h hero)]
                   (assoc h hero (conj facts fact))))))

(defn add-fact! [hero fact]
  (swap! heroes update-in [hero] conj fact))

(defn get-heroes []
  (sort (keys @heroes)))
