
(ns onlisp)

;2.2 Defining Functions
(defn doble [x] (* x 2)) 
(doble 1)
#'doble
(identical? doble (first (list doble)))
(fn [x] (* x 2))
(doble 3)
((fn [x] (* x 2)) 3)
;en clojure hay otra alternativa
(#(* %1 2) 3)
(def doble (fn [x] (* x 2)))

;2.3 Functional Arguments
(+ 1 2)
(apply + '(1 2))
(apply (fn [x y] (+ x y)) '(1 2))
(apply + 1 '(2))

(map #(+ %1 10) '(1 2 3))
(map + '(1 2 3) '(10 100 1000))

;2.4 Functions as Properties
;Este ejemplo se puede implementar con mapas
 (defn behave [animal]  ((get animal :behavior)))
 (def perro {:behavior #(prn 'menear-cola)})
 (behave perro)
 (def rata {:behavior #(prn 'chillar)})
 (behave rata)
 
;from http://blog.fogus.me/2008/10/02/on-lisp-clojure-chapter-2-redux/
(defn remove-if [f lst]
  (if (seq lst) ; idiomatic
    (if (f (first lst))
      (recur f (rest lst))
      (lazy-seq (cons (first lst) (remove-if f (rest lst)))))
    nil))

;2.5 Scope
(let [y 7] (defn test-scope [x] (list x y)))
(test-scope 3)
(let [y 5] (test-scope 3))

;2.6 Closures
(defn list+ [lst n] (map #(+ %1 n) lst))
(list+ '(1 2 3) 10)

;from http://blog.fogus.me/2008/10/02/on-lisp-clojure-chapter-2
;; This is surprisingly difficult given that Clojure does not allow the
;; modification of local variables defined in a let.  Only vars (globals)
;; and class fields can be modified with the (set!) function.  
;; Therefore, I had to hack it so that the closed counter is an array
;; of one integer element.  Therefore, it is the array that is modified
;; and not the count.  The point being, if you want to create closures
;; for functions modifying local state, then you have to use a mutable object
;; to do so
(let [counter (to-array [0])]
  (defn new-id [] (aset counter 0 (inc (aget counter 0))))
  (defn reset-id [] (aset counter 0 0)))
  
; No se pueden modificar variables locales en clojure asi que hay
; que usar ref y hacerlo variable local para que las closures lo capten
 
 (let [_counter (ref 0)]
	(defn new-id [] (dosync (ref-set _counter (+ @_counter 1))))
	(defn reset-id [] (dosync (ref-set _counter 0)))) 
(prn (new-id) (new-id)
(reset-id) (new-id))
	
(defn make-adder [n]
  #(+ %1 n))
 (def add2 (make-adder 2))
  (add2 1)
  
 ;; Al igual que antes no podemos modificar las variables locales
 ;; y usarlas como campos privados en una closure. 
(let [_counter (ref 0)]
  (defn make-adderb [m]
    (dosync (ref-set _counter m))
    (fn [x & change]
      (if change
        (dosync(ref-set _counter x))
        (+ @_counter x)))))

(def addx (make-adderb 1))
(addx 3)
(addx 100 true)
(addx 3)

;; db tambien tiene que ser reescrita en esa forma
(defn make-dbms [map]
	(if-let [refmap (ref map)]
		(list (fn [key] (@refmap key))
			(fn [key val] (dosync (ref-set refmap (assoc @refmap key val))))
			(fn [key val] (dosync (ref-set refmap (dissoc @refmap key val)))))
		(prn map "debe ser un map")))
		
(def cities (make-dbms {:london 'england}))
(prn (apply (first cities) [:london])
(apply (second cities) [:madrid 'spain])
(apply (first cities) [:madrid]))

;;;;;;;;;;;;;;;;;;;;;
;2.7 Local Functions;
;;;;;;;;;;;;;;;;;;;;;

;;Este ejemplo puede utilizarse con let
;;o mejor con letfn ya que con let las funciones locales
;;son visibles para ellas mismas y para el resto de asignaciones
;;al contrario que en CL. Para usar la recursion optimizada
;;usamos un loop
(defn count-instances [obj lsts]
  (letfn [(instances-in [lst]
    (loop [cnt 0 _lst lst]
		(if (empty? _lst) cnt
			(recur (if (= obj (first _lst)) (inc cnt) cnt) 
				(rest _lst)))))]
  (map instances-in lsts)))

(defn ocurrencies 
	[item & seqs] 
	(letfn [(ocurrs [cnt seq] 
			(if (empty? seq) cnt
			(recur 
				(if (= item (first seq))
					(inc cnt) cnt) 
				(if (seq? (first seq)) (first seq) (rest seq)))))]
	(reduce ocurrs 0 seq)))

(defn ocurrencies2 
	[item seqs]
		(letfn [(ocurrs [seq] (reduce #(if (= item %2) (inc %1) %1) 0 seq))]
			(map ocurrs seqs)))

(defn ocurrencies3 [item seqs]
	(letfn [(ocurrs [seq] (count (filter #(= item %1) seq)))]
		(map ocurrs seqs)))  

(time (prn 'count-instances (apply + (count-instances 'a '((a b c) (d a r p a) (d a r) (a a))))))
(time (prn 'ocurrencies (ocurrencies 'a '((a b c) (d a r p a) (d a r) (a a)))))
(time (prn 'ocurrencies2 (apply + (ocurrencies2 'a '((a b c) (d a r p a) (d a r) (a a))))))
(time (prn 'ocurrencies3 (apply + (ocurrencies3 'a '((a b c) (d a r p a) (d a r) (a a))))))

(time (prn 'count-instances (apply + (count-instances 3500 (list (range 1 200000))))))
;(time (prn 'ocurrencies (ocurrencies 3500 (list (range 1 200000)))))
(time (prn 'ocurrencies2 (apply + (ocurrencies2 3500 (list (range 1 200000)))))) ;la mas rapida
(time (prn 'ocurrencies3 (apply + (ocurrencies3 3500 (list (range 1 200000))))))
; Pendiente implmentar una version que acepte varias listas
; y listas anidadas

;;;;;;;;;;;;;;;;;;;;
;2.8 Tail-Recursion;
;;;;;;;;;;;;;;;;;;;;

;Esta version no es tail-recursive
(defn our-length [lst]
	(if (empty? lst) 0
		(+ 1 (our-length (rest lst)))))
;Comprobamos lo mal que le sienta a la stack
;A mi con este rango ya me salta
(our-length (range 1 5000))

;Esta si que es recursiva "en la cola"
;pero chafa la stack de clojure la optimizacion
;no es automatica
(defn our-find-if [fn lst]
(if (not-empty lst)
	(if (fn (first lst))
		(first lst)
		(our-find-if fn (rest lst)))))
(our-find-if #(> %1 5665) (range 0 10000))
; usando recur ya aguanta
(defn our-find-if [fn lst]
(if (not-empty lst)
	(if (fn (first lst))
		(first lst)
		(recur fn (rest lst)))))
(our-find-if #(> %1 5665) (range 0 10000))
;versiones tail-recursiva 
;la version mas clara y concisa creo que sera con loop
(defn our-length [lst]
	(loop [_lst lst acc 0]
		(if (empty? _lst) acc
		(recur (rest _lst) (inc acc)))))
(our-length (range 1 5000))

(defn triangle [n]
  (loop [c 0 _n n]
    (if (zero? _n)
      c
      (recur (+ _n c)
           (- _n 1)))))
(triangle 10)
(time (prn (triangle 30000000)))
;450000015000000
;"Elapsed time: 12822.784181 msecs"
;nil

;;;;;;;;;;;;;;;;;;;;;;;
;3.1 Functional Design;
;;;;;;;;;;;;;;;;;;;;;;;

(defn good-reverse 
	[lst] 
	(if (nil? lst) nil
	(loop  [_lst lst res '()]
		(if (empty? _lst) res
		(recur (rest _lst) (cons (first _lst) res))))))

; Clojure doesnt provide multiple value bindings in the same way that Lisp does. 
; Instead, you can construct a vector of values and then deconstruct them easily on the return within a let)

(defn mytrunc [num]
  [(int num) (- num (int num))])

(let [[int_part frac_part] (mytrunc 26.21875)]
  (str int_part " and " frac_part))

;A language stamps its pattern on
;our thoughts: someone used to programming in an imperative language may have
;begun to conceive of programs in imperative terms, and may actually find it easier
;to write imperative programs than functional ones. This habit of mind is worth
;overcoming if you have a language that will let you.

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;4.3 Operations on Lists;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;pg 42
;En clojure-contrib.seq-utils  ya hay una funcion similar a find2:find-first

(defn find-first
  "Returns the first item of coll for which (pred item) returns logical true.
  Consumes sequences up to the first match, will consume the entire sequence
  and return nil if no match is found."
  [pred coll]
  (first (filter pred coll)))

;pg 45
;En clojure last ya devuelve solo un elemento
(last '(1 2 3 4))

;version mas elaborada de single (single nil) es 
(defn single 
	([arg] (when-let [sq (seq arg)] (not (seq (rest sq))))))

;version lazy de concat con varios argumentos
(defn concat1 [lst & obj] (concat lst (seq obj)))

;mklist
(defn mklist [obj] (if (list? obj) obj (list obj)))

;longer .no he encontrado esta funcion ni en core 
;ni en contrib
(defn longer [x y] 
	(when-let [[_x _y] [(seq x) (seq y)]]
	(letfn [(compare [x y] 
			(and (not (nil? (first x))) 
				(or (nil? (first y)) (recur (rest x) (rest y)))))]
		(compare _x _y)) (> (count x) (count y))))
	
	
;me temo que la opcion count es mas efectiva en clojure
;solo con valores muy pequeños de la lista mas pequeña 
;se igualan 

(time (longer (range 1 10000) (range 1 5)))
;"Elapsed time: 0.102247 msecs"
;true
(time (> (count (range 1 10000)) (count (range 1 5))))
;"Elapsed time: 0.10979 msecs"
;true

;filter ya esta definido en clojure
(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (let [step (fn [p c]
                 (when-let [s (seq c)]
                   (if (p (first s))
                     (cons (first s) (filter p (rest s)))
                     (recur p (rest s)))))]
    (lazy-seq (step pred coll))))

;group en clojure es partition
(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap."
  ([n coll]
     (partition n n coll))
  ([n step coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (let [p (take n s)]
        (when (= n (count p))
          (cons p (partition n step (drop step s)))))))))


;;from clojure.contrib.seq-utils
;; 'flatten' written by Rich Hickey,
;; see http://groups.google.com/group/clojure/msg/385098fabfcaad9b
(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))
 
; from twitters cgrand and fogus
(defn flatten-cgrand [s] (remove seq? (tree-seq seq? seq s)))
(defn flatten-fogus [x] (if (seq? x) (apply concat (map flatten-fogus x))(list x)))

 ;la version de onlisp (usando reduce en lugar 
;de doble recursividad y lazy-cat) seria

(defn flatten-ol [x]
  (letfn [(rec [x acc]
	       (cond (not (seq? x))  (cons x acc)
		     (empty? x) acc
		     true (recur (first x) (rec (rest x) acc))))]
    (lazy-seq(rec x nil))))

(time (flatten '(1 (2 3 (4 5)) 1 2)))
;"Elapsed time: 0.131023 msecs"
(time (flatten-ol '(1 (2 3 (4 5)) 1 2)))
;"Elapsed time: 0.100851 msecs"
(time (flatten-cgrand '(1 (2 3 (4 5)) 1 2)))
;"Elapsed time: 0.067606 msecs"

(def bigtree (for [x (range 1 100)] (cons (range 1 5) (range 1 500))))
; Como era de suponer cuanto mas grande y anidado el arbol la de rick
; sigue con duracion fija y las otras se disparan 

(time (and (doall (flatten bigtree))false))
;"Elapsed time: 341.804615 msecs"
(time (and (doall (flatten-ol bigtree))false))
;"Elapsed time: 52.491587 msecs"
(time (and (doall (flatten-cgrand bigtree))false))
;"Elapsed time: 238.799624 msecs"
(time (and (doall (flatten-fogus bigtree))false))
;"Elapsed time: 294.651136 msecs"

;Para el manejo de "arboles" en clojure existe un libreria
;especialmente dedicada muy potente

(require ['clojure.zip :as 'zip])

(defn prune [pred tree] 
  (loop [loc tree]
    (if (zip/end? loc) (zip/root loc) 
      (recur (zip/next 
	      (if (pred (zip/node loc)) 
		(zip/remove loc) loc))))))

(def data '[[a * b] + [c * d]])
(def dz (zip/vector-zip data))

(prune #(= %1 'b) dz)

;En esta implementacion de before tenemos pred opcional
;y solo nos devuelve el resultado esperado
(defn before 
  ([x y lst] (before x y lst =))
  ([x y lst pred]
     (when-let [s (seq lst)]
       (if (and (pred (first s) x)
		(pred (second s) y)) (lazy-seq s) 
	  (recur x y (rest lst) pred)))))

(before 1 2 '(2 4 3 5 1 3 1 2 3 5)) ;(1 2 3 5)
(before 1 2 '(1 3 2 1 4)) ;nil

;after es trivial con nuestra aprox a before
(defn after 
  ([x y lst] (before y x lst))
  ([x y lst pred] (before y x lst pred)))


(after 2 1 '(3 4 a w 2 1 2 w))

;No he encontrado member como tal en clojure.core o contrib
(defn member [x sq]
  (when (seq sq)
    (if (= x (first sq))
      sq
      (recur x (rest sq)))))

;member implementado con drop-while
(defn member 
  ([x sq] (member x sq =))
  ([x sq pred] 
     (when (seq sq)
       (drop-while #((complement pred) x %1) sq))))

(member 'a '(1 3 4 a s r 2))

(defn duplicate [x lst] 
  (member x (rest (member x lst)))) 

(duplicate 'a '(3 1 a v g 4 3 a f 3 4))

;split-if es similar a split-with pero con
;el predicado que separa a la inversa

(defn split-if [pred lst]
  (split-with (complement pred) lst))

(split-if #(> %1 4) '(1 2 3 4 5 6 7)) 

;como ejercicio de traduccion e implementacion
;de clojure casi ya nos vale..lo unico una
;implementacion desde rick hickey pasando por bogus

(defn best [f xs] (reduce #(if (f %1 %2) %1 %2) xs))

;5.- Devolviendo funciones

;La posibilidad de devolver funciones refuerza la posibilidad
;de abstraccion.

;Clojure tiene implmentaciones en el nucleo de complementar y 
;componer funciones ( f ◦g(x) = f (g(x)) )

((comp #(+ %1 2) #(* %1 %2)) 4 2) ; (+ (* 4 2) 2)= 10 

;La opcion "memoize" guardando los resultados ya calculados
;en una cache si el tiempo de calculo es grande esta cubierta
;por el "lazyness" de clojure. Sin embargo tb existe ememoize en contrib

(defn memoize
  [function]
  (let [cache (ref {})]
    (fn [& args]
      (or (@cache args)
          (let [result (apply function args)]
            (dosync
             (commute cache assoc args result))
            result)))))

;Las funciones de recursion sobre arboles estan contempladas
;en la libreria zip

;La funcion lrec no esta como tal en el api del nucleo
;o en contrib, o no la encuentro
;Una funcion que devuelve funciones que recorren una lista
;de forma recursiva.
;La tranformacion de la funcion parece que seria mas limpia
;y elegante con una macro, por otro lado la funcion resultante
;no es recursiva por la cola.

(defn lrec [rec base] 
  (fn this [lst] 
    (if (empty? lst)
      (if (fn? base)(base) base)
      (rec (first lst) #(this (rest lst))))))

(def ourlength (lrec #(inc (%2)) 0))

;crearia la siguiente funcion
(def ourlength (fn this [lst] 
  (if (empty? lst) 0 (inc (this (rest lst))))))

(ourlength '(1 2 3))

(def ourevery  (lrec #(and (> %1 2) (%2)) true))

;esta seria la funcion tranformada
(def ourevery (fn this [lst]
  (if (empty? lst) true 
      (and (> (first lst) 2) 
	   (this (rest lst))))))

(ourevery '(7 3 3 4 5 6))

;"In general, composing and combining functions is more easily and efficiently
;done with macros." pg 75

;Capitulo 6: Funciones como representaciones

;En este capitulo PG nos comenta la posibilidad de usar las closures
;como "representaciones de la realidad" al igual que las estructuras
;de datos tales como listas o arboles

;Si las closures pueden referirse a otras closures se pueden usar como
;punteros (p. ej c) pero mas elaborados.

;Closures have three useful properties: they are active, they have local state, and
;we can make multiple instances of them. 
;[...] As well as having its own local state, a closure can refer to another closure.
;Se pueden construir "redes" o sea linked lists, arboles, grafos etc

;Las ventajas de los closures son como muy parecidas a los objetos.
;En clojure el "estado" local de los closures es inmutable por defecto
;asi que tendriamos que usar refers para mutar el estado local de las
;closures.

(defstruct node :contents :yes :no)

(def *nodes* {})

(defn defnode 
  ([name conts] (defnode name conts nil nil))
  ([name conts yes no]
     (def *nodes* 
	(assoc *nodes*  name
	       (struct node conts yes no)))))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defn run-node [name]
  (let [node (name *nodes*)]
    (prn (:contents node))
    (when (:yes node)   
      (if (= (read) 'yes) 
	(run-node (:yes node))
	(run-node (:no node))))))

;esta es la forma clasica...separando los datos del codigo
;que los usa, PG nos da dos versiones mas del programa,
;usando closures como estructuras de datos con codigo
;Al conseguir un resultado similar a la oop que tanto critica
;vamos a saltarnoslo

;Capitulo 8: Macros
;La definicion de macro es una funcion que genera codigo lisp
;Una funcion(expresion) produce resultados una macro produce 
;una expresion que al ser evaluada produce resultados.
;Tiene dos fases: una de expansion y otra de evaluacion

(defmacro nil! [x] (list 'def x nil)) 
(macroexpand '(nil! y))
;(def y user/null)

;Traducido a lenguaje comun: "donde te encuentres la expresion
;(nil! y) sustituyelo por la expresion (def y user/null) antes
;de evaluarlo
(nil! y)

;A macro call occurring in the definition of a function will be expanded when the
;function is compiled, but the expansion—or the object code which results from
;it—won’t be evaluated until the function is called.

;Muchos lenguajes poseen algo similar a una macro, pero las de Lisp son 
;singularmente poderosas. Cuando un fichero con codigo Lisp es compilado
;un parser lee el codigo fuente y envia su salida al compilador.
;Aqui esta el toque genial : la salida del parser consiste en listas de 
;de objetos lisp. Con las macros, podemos manipular el programa
;en el estado intermedio entre el parser y el compilador. 

;7.2 Contracomilla
;Con la contracomilla se pueden crear plantillas de expresiones lisp.
;Su uso mas comun es en las macros
;(de clojure.org) Para todas las forms que nos sean Symbols, Lists, Vectors y Maps, 
;Para los simbolos la contracomilla "resuelve" el simbolo y lo convierte
;en un simbolo totalmente cualificado
;`x es lo mismo que 'x.
`(a b c)
;(user/a user/b user/c)
'(a b c)
;(a b c)

;La utilidad se acentua con , y ,@ (en lisp) ~ y ~@ en clojure
;Dentro de una plantilla (una lista, un mapa, un vector con la 
;contracomilla delante) todos los elementos estan quoteados
;salvo los elementos que tienen ~ y ~@ que son evaluados y su 
;valor colocado en la plantilla.

(def l '(a b c))
(def a 1)
`(prn a ~a l ~l ~@l)
;(clojure.core/prn user/a 1 user/l (a b c) a b c) 

;Las ~ contrarrestan los ` asi que su numero debe ser correlativo
;The general rule is: a comma surrounded by n commas must
;be surrounded by at least n+1 backquotes.
(def b 2)
(def c 3)
`(~a ~(prn `~c))
; 3 nil

;En este caso la ultima ~ esta rodeada por un ~ y dos `
  
;La primera macro se puede reescribir con `

(defmacro nil! [x] (list 'def x nil))  
(defmacro nil! [x] `(def ~x nil))

;La diferencia se nota mas cuanto mas grande es la macro
;Por ejemplo nif que es un if ternario dependiendo
;de si el parametro numerico es positivo 0 o negativo

;Usando cond de clojure.core
(defmacro nif [expr pos zero neg]
  `(cond (pos? ~expr) ~pos
	 (neg? ~expr) ~neg
	 (zero? ~expr) ~zero))

(map #(nif %1 'P 'Z 'N) '(-2 0 1))

;Usando case de clojure.contrib.fcase
(use 'clojure.contrib.fcase)
(defmacro nif [expr pos zero neg]
  `(case (compare ~expr 0)
     1 ~pos 0 ~zero -1 ~neg))

;Usando "list" es mucho mas largo y complejo
;pues la expansion es casi igual a su definicion

(defmacro nif [expr pos zero neg]
  (list 'case (list 'compare expr 0)
     1 pos 0 zero -1 neg))

;En este caso la macro es innecesaria 
(defn nif [expr pos zero neg]
     (case (compare expr 0)
	   1 pos 0 zero -1 neg))

;@~, unquote-splicing es una variante de ~. Si la expresion
;es una lista la inserta a la vez que elima los parentesis
;mas exteriores

(def l '(1 2 3))
`(a ~l b c)  
;(user/a (1 2 3) user/b user/c)
`(a ~@l b c)  
;(user/a 1 2 3 user/b user/c)

;Restricciones:
;1.- El ~@ debe utilizarse dentro una secuencia
;2.- EL ~@ debe utilizarse con una lista (en lisp puede
;    utilizarse en el ultimo elemento de una lista)

;Se suele utilizar cuando en una macro se toman un 
;numero indeterminado de parametros y se pasan
;a otra macro o funcion que utiliza un numero
;indeterminado de parametros
;Esta situacion aparece en bloques implicitos, que 
;no suelen utilizarse en el codigo sino dentro de macros

(defmacro our-when
  [test & body]
  `(if ~test (do ~@body)))

;El efecto de ~@ puede reproducirse con '
;como se hace en clojure.core con when

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  [test & body]
  (list 'if test (cons 'do body)))

;` y ~/~@ se pueden utilizar fuera de las macros

(defn saludar [& name] `(hello ~@name))
(saludar 'jose 'pedro)

;7.3.- Definir macros sencillas
 
;El metodo: se escribe una llamada a la macro que queremos
;definir y luego en como quedaria la expansion

;Ejemplo memq que hace un member con = como funcion de 
;pertenencia

;call: (memq x choices?
;expansion: (member x choices =)

;con la llamada tenemos la firma (defmacro memq [obj lst] ..
;empezamos el cuerpo con ` y leemos expresion por expresion
;poniendo los parentesis igual.Para cada expresion  si 
;esta en la llamada se pone con unquote si no se pone sin mas.

(defmacro memq [obj lst] 
  `(member ~obj ~lst =))

;7.4.- Probando macros

;Para probar las macros se las expande con macroexpand y 
;y macroexpand-1 y luego se observa o se evalua por partes

;7.5.- Destructuracion de parametros

;La destructuracion es una generalizacion del orden de asignacion
;de las llamadas a la funciones. En la llamada a una funcion
;los valores en la llamada se asignan a los parametros en orden
;La destructuracion describe la situacion donde
;este tipo de asignacion posicional se hace siguiendo cualquier
;estructura de listas al igual que listas simples como (x y z)

;En clojure let y fn usan destructuracion por defecto en los params.
;Let seria una especie de destructure-bind

(let [[x [y] & z] '(a (b) c d)] (list x y z))
;(a b (c d))

;En clojure hay dos tipos uno sobre secuencias 
;como vectores, listas, seqs, s, arrays, y cualquier cosa
;que implemente nth y otra sobre mapas y otras estructuras
;asociativas como vectores, strings y arrays (tienen keys
;enteros)

;La marca & x (en lisp . x) significa que ese variable
;recoge el resto de parametros aun no conectados.
;En clojure la clave as: hace que la variable 
;se conecte con toda la expresion de inicializacion.

(let [[a b c & d :as e] [1 2 3 4 5 6 7]]
  [a b c d e])

;[1 2 3 (4 5 6 7) [1 2 3 4 5 6 7]]

;Las asignaciones de mapas tambien usan la clave :or
;que establece valores por defecto de las keys

(let [{a :a, b :b, c :c, :as m :or {a 2 b 3}} {:a 5 :c 6}]
  [a b c m])

;[5 3 6 {:a 5, :c 6}]

;En lisp la destructuracion permite separar los argumentos
;del cuerpo de expresiones en una macro con parentesis
;EN lisp la distincion se hace usando corchetes para los
;parametros

;7.7.- Las macros como programas 
;Una aproximacion mas general a la implementacion de macros es pensar en el
;tipo de expresion que tu quieres usar, en que quieres que se expanda y entonces
;escribir el prgorama que transforma la primera en la segunda

;8.- Cuando usar macros
;Por defecto hay que usar funciones. es ineficiente y poco elegante usar
;macros cuando se pueden usar funciones.
;Las macros pueden hacer algo que no puede hacer las funciones: controlar
;la evaluacion de las expresiones y expandirlas en el contexto adecuado.
;Dependiendo de donde se colocan en la expansion, pueden ser evaluados
;una vez, varias veces o ninguna.
;Las macros efectuan este control de cuatro formas:
;1.- Transformacion.
;2.- Ligado
;3.- Evaluacion condicional, como en la macro when.
;4.- Evaluacion multiple.
;5.- Usar el contexto de la llamada. Ciertos valores de la macro
;dependen del contexto (aka variables globales). No se recomienda
;este uso de las macros aunque puede ser util en ciertas circunstancias
(def y 5)
(defmacro foo [x] `(+ ~x y))
(foo 4)
;7.-Rodear con un nuevo contexto. La macro hace que los argumentos se ejecuten
;en un nuevo contexto lexico.

;8.2.- ¿Funciones o macros?
;El caso facil de eleccion de la macro es el descrito anteriormente.
;Cualquier operador que necesita acceder a los parametros antes de
;ser evaluados deben ser escritos como una macro porque no hay eleccion
;Pero hay casos en que no esta tan claro y se pueden implementar ambas
;Por ejemplo avg (media)
(defn avg [& args] (double (/ (apply + args) (count args))))
(defmacro avg [& args] `(double (/ (+ ~@args) ~(count args)))) 

;Pros y contras de usar macros:
;1.-Computacion en tiempo de compilacion. En lisp la expansion de la macro 
;es en tiempo de compilacion, asi que todo los que se haga en la macroexpansion
;es ahorro de tiempo de ejecucion.
;2.-Sometimes, using macros instead of functions will
;make a program more closely integrated with Lisp. Instead of writing a
;program to solve a certain problem, you may be able to use macros to
;transform the problem into one that Lisp already knows how to solve. This
;approach,when possible, will usually make programs both smaller andmore
;efficient: smaller because Lisp is doing some of your work for you, and
;more efficient because production Lisp systems generally have had more
;of the fat sweated out of them than user programs. This advantage appears
;mostly in embedded languages.
;3.-...

;Los contras:
;4.-Las funciones son datos mientras que las macros son como instrucciones
;al compilador. La funciones pueden pasarse como parametros, ser devueltas
;por funciones y guardarse en estructuras de datos. No se puede hacer eso
;con las macros. Se puede rodear una macro con un lambda y pasarla
;como parametro a apply o map
(apply #(avg %1 %2 %3) [3 4 5])
;5.-Claridad del codigo fuente.
;6.-Claridad del codigo en ejecucion
;7.-Recursion. No es tan sencilla en las macros como en las funciones
  
