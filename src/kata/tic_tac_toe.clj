(ns kata.tic-tac-toe)

;; field interface
(def empty-field " ")
(def player-owner [field] field)

;; board interface
(defn rows [board] board)
(defn cols [board]
  (apply map vector board))
(defn diagonals [board]
  (map (fn [rows] (map-indexed #(%2 %1) rows))
       [(rows board) (reverse (rows board))]))
(defn all-fields [board] (apply concat board))

;; rules for game over 
(defn taken? [field]
  (not= (id field) id-empty-field))
(defn all-taken? [fields]
  (every? taken? fields))
(defn taken-by-player? [fields]
  (and (all-taken? fields)
       (apply = (map id fields))
       (id (first fields))))
(defn is-over? [{board}]
  (or (all-taken? (all-fields (:board game)))
      (let [all (apply concat (juxt rows cols diagonals)
                       (:board game))]
        (some taken-by-player? all))))

(defn choose-field [game]
  (let [guess (repeat 2 (inc (rand-int (dec (count game)))))]
    (if (taken? (get-field game guess))
      (choose-field player game) guess)))
(defn get-field [game loc]
  (get-in game loc))
(defn update-field [player game loc]
  (update-in game loc #(id player)))0
(defn take-field [player game]
  (let [loc (choose-field player game)]
    (when (not (taken? (get-field game loc)))
      (update-field player game loc))))

(defn take-turn [player game]
  (if (is-over? game) game
      (take-field player game)))
        
(def game (vec (repeat 3 (vec (repeat 3 id-empty-field)))))
