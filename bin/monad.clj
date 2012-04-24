(ns monad)

(defprotocol monad/Runnable
  (run [r]))

(defprotocol Monad
  (>>= [m f]))

(defprotocol Monad+
  (mzero [m])
  (mplus [m1 m2]))

(defn >> [m k] (>>= m #(k)))

(deftype Identity [value]
  Monad
  (>>= [m f]  (f value))
  Runnable
  (run [m] value))

(deftype Maybe [value]
  Monad
  (>>= [m f]
       (if-let [n value]
         (f n) (Maybe. nil))))

(extend Maybe
  Monad+
  {:mzero (fn [_] (Maybe. nil))
   :mplus (fn  [m1 m2]
            (Maybe.
             (let [[v1 v2] [(.value m1) (.value m2)]]
               (if (nil? v1) (when (comp not nil? v2) v2)
                   v1))))})

(defn from-just [m]
  (if (nil? (.value m))
    (throw (new IllegalArgumentException
                "Not nil allowed"))
    (.value m)))

(defn from-maybe [default m]
  (if (nil? (.value m))
    default
    (.value m)))

(extend clojure.lang.PersistentList
  Monad
  {:>>= (fn [xs f] (mapcat f xs))})

(defn monad [v] (:monad (meta v)))

(defmulti return monad) 
(defmethod return Maybe [mvalue] (Maybe. (:value mvalue) ))
(defmethod return Identity [mvalue] (Identity. (:value mvalue)))
(defmethod return clojure.lang.PersistentList
  [mvalue] (list (:value mvalue)))

(defn mval [m v]
  (with-meta {:value v} {:monad m}))

(defn lift-m
  ([f] (comp #(>>= %1 f)  return))
  ([f & fs] (comp #(>>= %1 f) (apply lift-m fs))))
