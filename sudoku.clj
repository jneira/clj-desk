;; sudoku

;; $Id$
;; Sudoku solver by Tzach
;; with the help of Konrad and Keith
;; now with GUI and a new problem generator!


(ns sudoku
  (:require [clojure.zip :as zip])
  (:gen-class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; generic util functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mod3range [x]
  (let [start (- x (rem x 3))]
       (range start (+ 3 start))))
       
(defn shuffle [coll]
	(if (<= (count coll) 1) coll
 	(let [[prefix remainder] (split-at (rand-int (count coll)) coll)]
      	(concat (list (first remainder)) (shuffle (concat prefix (rest remainder)))))))

(defn replace-switch [seq a b]
 	"Made this function only to write abba in the code ;)"
 	(replace {a b b a} seq))

(defn update-zip [tree f]
	"change element in a nested structure by execute f on each node value"
   (loop [loc (zip/vector-zip tree)]
     (if (zip/end? loc)
         (zip/root loc)
         (recur (zip/next
         		(let [v (zip/node loc)]
         			(if (not (zip/branch? loc))
                (zip/replace loc (f v))
                 loc)))))))

(defn switch-zip [tree pairs]
"switch element in a nested structure by a list of pairs:
	e.g. (switch-zip [1 [1 2]] { 1 3 }) => [3 [3 2]]"
	(update-zip tree 
		#(let [m (get pairs %)]
				(if m m %))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sudoku logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn print-board [board]
  "Pretty print the sudoku board"
  (println)
  (doseq [row board] (println row)))


(defn neighbors-pos [pos]
  "return a collection of neighbors positions to pos"
  (remove #(= pos %)
	  (distinct (concat
		     (for [y (range 3) z (range 3)] [(get pos 0) y z]) ; row neighbors
		     (for [x (range 9)] [x (get pos 1) (get pos 2)]) ; col neighbors
		     (for [x (mod3range (get pos 0)) z (range 3)] [x (get pos 1) z]))) ; square neighbors; 
	  ))

(def neighbors-pos (memoize neighbors-pos))

(defn neighbors-values [board pos]
      "return a list of neighbor positions values"
      (map #(get-in board %) (neighbors-pos pos)))

(defn valid-values [board pos]
      "return a list of values which does not violate the neighbors values. return nil if the position already have a value" 
      (if (zero? (get-in board pos))
	  (clojure.set/difference (set (range 1 10)) (set (neighbors-values board pos)))
	  (seq nil)))

(defn map-board [board f]
      "execute function f on each of the position on board.  function f get the position and the board as parameters"
       (for [x (range 9) y (range 3) z (range 3)]
	     (let [pos [x y z]]
		  [pos (f board pos) ] 
		 )))

(defn next-cell [board]
  "return the next potential cell to set, and the valid alternatives"
  (let [w (map-board board valid-values)] 
   (first 
    (apply concat (for [n (range 1 10)]
    	(filter 
		     #(= n (count (second %))) w))))))

(defn complete? [board]
  (not (some #(second %) 
	     (map-board board (fn [board pos] (zero? (get-in board pos)))))))

(defn lazy-all-solutions [board]
	"return all valid solution to a sudoku problem
	if you do not have all day, use it in a lazy way"
  (lazy-seq
    (if (complete? board)
      (list board)
      (let [[pos valid-values] (next-cell board)]
               (apply concat (for [v valid-values]
              (lazy-all-solutions (assoc-in board pos v))))))))

(defn sudoku [board]
"solve a sudoku problem"
  (first (lazy-all-solutions board)))

(def *empty-board*
     (vec
      (for [x (range 9)]
	[[0 0 0] [0 0 0] [0 0 0]])))

(def *core-sudoku* (sudoku *empty-board*))

;;; use the solver: (print-board (sudoku (generate-board 3)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; generate new problems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn rand-board 
	"switch random two didigt in a board, repaet for num times"
	([board num]
	(if (<  num 1) board
	(let [a (inc (rand-int 9))
				b (inc (rand-int 9))]
		(recur (switch-zip board { a b b a }) (dec num)))))
	([board] (rand-board board 10)))
	
(defn make-problem [board num]
	"replace num of the digits in the board by zero"
	(if (< num 1) board
		(recur (assoc-in board [(rand-int 9) (rand-int 3) (rand-int 3)] 0) (dec num))))
		
(defn generate-board [hard]
	"creatre s new proble. Hard is a degree on the scale of 1 to 4" 
	(make-problem (rand-board *core-sudoku*) (* 20 (+  2 hard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sudoku GUI 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import '(javax.swing JFrame JTextField  JOptionPane JMenuItem JFileChooser JFormattedTextField)
	'(javax.swing.text MaskFormatter)
        '(java.awt.event ActionListener)
	'(java.awt ComponentOrientation)
	'(javax.swing.event MenuListener)
        '(java.awt GridLayout Component FileDialog))

(use 'clojure.contrib.duck-streams)

(defn components [container]
  "return a sequnce of a swing components"
  (for [i (range (.getComponentCount container))]
    (.getComponent container i)))

(defn parse-integer [str]
  (try (Integer/parseInt str) 
       (catch NumberFormatException nfe 0)))

(defn abs [x]
  (if (> 0 x) (- x) x))


(defn parse-digit [str]
  (abs (rem (parse-integer str) 10)))


(defn get-gui-board [rows]
  "convert a the sudoko GUI to a vector base board"
  (vec (map vec
	    (for [r rows]
	      (map vec
		   (partition 3
			      (for [text (components r)]
				 (parse-digit (.getText text)))))))))

(defn pairs [a b]
  "return a sequnce of pairs from elements of a , b"
  (for [[x y] (partition 2 (interleave a b)) ] (list x y)))

(defn set-gui-board [board rows]
  "set a gui board from values in board"
  (doseq [[x r] (pairs board rows)]
    (doseq [[v t] (pairs (apply concat x) (components r))]
      (doto t
	(.setText (if (zero? v) nil (str v)))
	(.setCaretPosition 0)))))
    
(def *about-text*
     " Sudoku Solver is a demo application\n created by Tzach as an excersize in Clojure\n 2009")

(defmacro add-item-action [item action]
  "add a action as a listener listener to a item"
  `(.addActionListener ~item 
		      (proxy [ActionListener] []
			(~'actionPerformed [~'evt] ~action))))

(defn msg-box [text]
  (JOptionPane/showMessageDialog nil, text))

(defn create-board-gui ([]
  "create a Sudoku GUI"
  (let [frame (javax.swing.JFrame. "Sudoku Solver v15")
	rows (for [x (range 9)] (javax.swing.JPanel. (java.awt.GridLayout. 1 9)))
	bar (javax.swing.JMenuBar.)
	solve-item (JMenuItem. "Solve")
	open-item (JMenuItem. "Open")
	save-item (JMenuItem. "Save")
	clear-item (JMenuItem. "Clear")
	about-item (JMenuItem. "About")
	exit-item (JMenuItem. "Exit")
	easy-item (JMenuItem. "Easy")
	medium-item (JMenuItem. "Medium")
	hard-item (JMenuItem. "Hard")
	file-menu (javax.swing.JMenu. "File")
	new-menu (javax.swing.JMenu. "New Problem")
	file-chooser (javax.swing.JFileChooser. ".")
	]

    (add-item-action about-item (msg-box *about-text*))
    (add-item-action solve-item (let [solution (sudoku (get-gui-board rows))]
				  (if solution (set-gui-board solution rows)
				      (msg-box "Impossible!\n are you sure the input is valid?"))))
    (add-item-action exit-item (.dispose frame))
    (add-item-action save-item (do 
				 (.showSaveDialog file-chooser frame)
				 (let [file (.getSelectedFile file-chooser)]
				   (when file (spit file (get-gui-board rows))))))
    (add-item-action open-item (do 
				 (.showOpenDialog file-chooser frame)
				 (let [file (.getSelectedFile file-chooser)]
				   (when file (set-gui-board (load-string (slurp* file)) rows)))))
    (add-item-action clear-item (set-gui-board *empty-board* rows))
    (add-item-action easy-item (set-gui-board (generate-board 1) rows))
    (add-item-action medium-item (set-gui-board (generate-board 2) rows))
    (add-item-action hard-item (set-gui-board (generate-board 3) rows))
    
    (doseq [r rows] 
      (do
	(doseq [x (range 9)]
	  (let [t (JFormattedTextField. (MaskFormatter. "#"))]
	    (.setHorizontalAlignment t JTextField/CENTER)
	    (.add r t)))
	(.add frame r)))
    (doto file-menu
      (.add solve-item)
      (.add open-item)
      (.add save-item)
      (.add clear-item)
      (.add about-item)
      (.add exit-item))
    (doto new-menu
    	(.add easy-item)
    	(.add medium-item)
    	(.add hard-item))      
    (doto file-chooser 
      (.setDialogTitle "Soduko File"))
    (doto bar
      (.add file-menu)
      (.add new-menu))
    (doto frame
      (.setLayout (java.awt.GridLayout. 9 1))
      (.setSize 300 300)
      (.setJMenuBar bar)
      (.applyComponentOrientation ComponentOrientation/LEFT_TO_RIGHT)
      (.setVisible true))
      rows))
  ([problem] (set-gui-board problem (create-board-gui))))

;; use
;; (create-board-gui)

  
(defn -main [] (create-board-gui (generate-board 1)))