(ns course
  [:use book-list clojure.contrib.seq-utils
   [clojure.contrib.duck-streams :only (to-byte-array)]]
  [:require [clojure.contrib.str-utils2 :as s]])

(defn print-book
  "Prints out information about a book."
  [ {:keys [title authors price] :as book } ]
  (println "Title:" title)
  (let [[first second & more] authors]
    (println "  Author: " (comma-sep
                           (filter seq [first
                                        second
                                        (when more "et. al.")]))))
  (println "  Price:" (str price))
  (println "  Raw:" (pr-str book)))
    

(defn print-books [& books]
  (let [f (flatten books)] 
    (dorun ( map print-book f))
    (print   "\n=============\n")
    (println "Printed information on"
             (count f)
             "books.")))

(def cities [{:city "Vienna" :temp 55} {:city "Krakow" :temp 52} {:city "Pune" :temp 85} {:city "Houston" :temp 57}])

;   1. Write a program that outputs the average temperature of the above cities rounded to 2 decimal places.
;   2. Since two of the four cities are in Europe, also add a function to convert temperatures between Fahrenheit to Celsius.

(defn avg-temp [cities]
     (.setScale
      (/ (apply + (map :temp cities)) (BigDecimal. (count cities)))
      2 (java.math.RoundingMode/CEILING)))

(defn temp-average [cities]
  (with-precision 2 :rounding UNNECESSARY
                  (.decimalValue (/ (apply + (map :temp cities)(count cities) )))))

(defn c-to-f [c] (+ 32 (* 1.8 c)))
(defn f-to-c [f] (/ (- 32 f) 1.8))

; nice use of -> 
(defn celsius-to-fahrenheit [temp]
  (-> temp
      (* 9)
      (/ 5)
      (+ 32)))

(defn fahrenheit-to-celsius [temp]
  (-> temp
      (- 32)1
      (* 5)
      (/ 9)))

(defstruct my-book :title :authors )

(def *library* (ref #{}))

(defn add-book
  ([book] (add-book *library* book))
  ([library book]
     (dosync (commute library conj book))))
 
(defn remove-book
  ([book] (remove-book *library* book))
  ([library book]
     (dosync (commute library disj book))))

(def x (ref 1))
(def y (ref 1))
(def z (ref 1))

(defn ok-to-inc? [x y]
  (< (+ x y) 3))

(comment do   (future
       (dosync
        (prn '***********)
        (prn 'Future1)
        (let [xv @x
              yv @y]
          (prn :x xv :y yv)
          ;; let's take a 1 second nap...
          (Thread/sleep 1000)
          (if (ok-to-inc? xv yv)
            (alter x inc))
          (prn :x @x :y @y)))

       (future
        (dosync
         (prn 'Future2)
         (let [xv @x
               yv @y]
            (prn :x xv :y yv)
           (Thread/sleep 1000)
           (if (ok-to-inc? xv yv)
             (alter y inc))
           (prn :x @x :y @y))))))

(comment
 "dosync explicitly does not  make sure that Refs which are read by the code
inside it won't be modified by other transactions. The only Refs for which such a
guarantee is provided are those for which the current transaction acquires some sort
of lock (strictly speaking, it's a guarantee that either no other transaction touches
our Refs or we retry).
The point here is that @x, for x a Ref, doesn't involve obtaining any sort of lock on x.
So if read from a Ref via @ = deref, but never modify it, your transaction will not pay
attention to other transactions pulling the old value of x from under your feet, as it were.
If you depend on having up-to-date knowledge of the value of x for the duration of your
transaction (actually: transaction try, meaning a single run through the transaction
ending in commit or retry), yet never need to modify it, you need to touch it with ensure
which, under the hood, obtains a read lock on it.
The key concept here is that of 'write skew'; see the Wikipedia entry on Snapshot isolation
for a definition and some examples.
Note that while I mention locks above, it is really sufficient to think about the transaction
'touching' the Ref in some way. Readers in Clojure normally do not 'touch' Refs in a manner
disturbing to anyone else who is interested in the given Refs; if a 'disturbing read)
is desired, ensure is the solution.)")

(do
  (future
   (dosync
    (let [xv @x
          yv (ensure y)]
      ;; let's take a 1 second nap...
      (Thread/sleep 1000)
      (if (ok-to-inc? xv yv)
        (alter x inc)))))
  (future
   (dosync
    (let [xv (ensure x)
          yv @y]
      (Thread/sleep 1000)
      (if (ok-to-inc? xv yv)
        (alter y inc))))))

(defmacro run-sync [& body] `(dosync (dorun ~@body)))

(defn reset-stuff []
      (run-sync
       (map ref-set
            [x y z] ; could pass in some arguments, of course
            [1 2 3])))

(def cols "ABCD")
(def rows (range 1 4))

(defn for-example []
 (dorun
  (for [col cols :when (not= col \B)
        row rows :while (< row 3)]
    (println (str col row)))))

; "x/y students passed (zz.zz%)"
(def results [0 0 0 1 0 1 1 1 1 0 1 1 0 1 0 0 1 1 1 0 0 1 0 1])

(defn student-report []
  (let [numpass (count (filter #(not= %1 0) results))
        numtot (count results)
        perc (* 100.00 (/ numpass numtot))] 
    (println (str numpass "/" numtot " students passed "  "(" perc "%)"))))

;W3E2: (Deaf Grandma)
(defn upper? [ch] (Character/isUpperCase ch))
(defn all-upper? [str] (reduce #(and %1 (upper? %2)) str))

(defn lowerCase [ch] (Character/isLowerCase ch))
(defn all-lowerCase? [str]
  (when (not-empty str) (every? lowerCase  str)))
(defn rand-int-between [from to]
  (+ (rand-int (- to from)) from))

(defn grandma-reply [word]
  (when (not-empty word)
             (if (all-lowerCase? word)
               "HUH?! SPEAK UP, SONNY!"
               (str  "NO, NOT SINCE "
                     (rand-int-between 1930 1950) "!"))))

(def word)
(defn grandma! []
  (binding [word ""]
    (while (not= word "BYE")
           (println (grandma-reply word))
           (set! word (read-line)))))

(defn grandma []
  (loop [word (read-line)]
    (when (not= word "BYE")
      (println (grandma-reply word))
      (recur (read-line)))))

;W3E4: (id3 tags)

(comment binding [*out* (java.io.FileWriter. "my.log")]
  (println "This goes to the file my.log.")
  (flush))

    



    
