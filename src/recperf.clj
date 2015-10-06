(ns recperf)

(defn tek [x y z]
  (if (<= x y) y
      (tek (tek (- x 1) y z)
           (tek (- y 1) z x)
           (tek (- z 1) x y))))

(defn test1 [n]
  (tek n 0 (+ n 1)))

;; (time (test1 10))
;; "Elapsed time: 938.490157 msecs"

;; Entiendo que los argumentos  no hay
;; que volver a envolverlos en una closure
;; ¿se me escapa algo?
(defn tek2 [x y z]
  (let [X(x) Y(y)]
    (if (<= X Y) Y
    (recur (fn [] (tek2 (fn [] (- X 1)) y z))
           (fn [] (tek2 (fn [] (- Y 1)) z x))
           (fn [] (tek2 (fn [] (- (z) 1)) x y))))))

(defn test2 [n]
  (tek2 (fn [] n) (fn [] 0) (fn [] (+ n 1))))

;; (time (test2 2000))
;; "Elapsed time: 3.02287 msecs"
;; (time (test2 3000))
;; stackoverflow

;; Para poder definir thunk necesito una macro
;; (que hace casi equivalente el codigo generado
;; por la macro al anterior aunque un poco mas rapido) 
(defmacro thunk [f] `(fn [] ~f))  

(defn tek3 [x y z]
  (let [X (x) Y (y)]
    (if (<= X Y) Y
        (recur (thunk (tek3 (thunk (- X 1)) y z))
               (thunk (tek3 (thunk (- Y 1)) z x))
               (thunk (tek3 (thunk (- (z) 1)) x y))))))

(defn test3 [n]
  (tek3 (thunk n) (thunk 0) (thunk (+ n 1))))

;;(time (test3 2000))
;; "Elapsed time: 1.999975 msecs"
;;(time (testf3 3000))
;; stackoverflow


;; con delay 
(defn tek4 [x y z]
  (let [X @x Y @y]
    (if (<= X Y) Y
        (recur (delay (tek4 (delay (- X 1)) y z))
               (delay (tek4 (delay (- Y 1)) z x))
               (delay (tek4 (delay (- @z 1)) x y))))))

(defn test4 [n]
  (tek4 (delay n) (delay 0) (delay (+ n 1))))

;; (time (test4 1000))
;; "Elapsed time: 1.851841 msecs"


(defn tek5 [x y z]
  (let [X (trampoline x) Y (trampoline y)]
    (if (<= X Y) Y
       (thunk (tek5 (thunk (tek5 (thunk (- X 1)) y z))
                    (thunk (tek5 (thunk (- Y 1)) z x))
                    (thunk (tek5 (thunk (- z 1)) x y)))))))

(defn test5 [n]
  (trampoline
   (tek5 (thunk n) (thunk 0) (thunk (+ n 1)))))
