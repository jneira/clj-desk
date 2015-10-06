(ns marvel
  (:require [clj-http.client :as http]
            [clojure.core.typed :as t]
            digest))

(def public-key "8c893919b9be9291cced99b01cd370c4")
(def private-key "ce0241c72edc7a3d70e656611172f697bf49c1bd")
(def base-url "http://gateway.marvel.com")
(def spec-url (str base-url "/docs/public"))
(defn get-json [path ps]
  (get-in (http/get path {:query-params ps :as :json}) [:body]))

(def spec (get-json spec-url {}))
(defn api-params
  ([] (api-params (System/currentTimeMillis)))
  ([ts] {:ts ts :apikey public-key
         :hash (digest/md5 (str ts private-key public-key))}))

(defn fns-data [spec base-url gps] 
  (apply merge-with into
     (for [a (:apis spec)
           :let [ts (.substring (:path a) 11) r #"/\{(.*?)\}"
                 name (.replaceAll ts (str r) "")
                 fps (mapv (comp symbol second) (re-seq r ts))
                 url (str base-url (.replaceAll (:path a) (str r) "/%s"))]]
       {name [`(~(conj fps 'lps) (get-json (format ~url ~@fps)
                                           (merge ~gps ~'lps)))]})))

(defmacro wrap [spec base-url gps]
  `(do ~@(for [[name fns] (apply fns-data (map eval [spec base-url gps]))
               :let [name (symbol (.replaceAll name "/" "-"))]]
           `(def ~name (fn ~@fns)))))


(defn models-types [{:models m}]
  (reduce-kv  model-type {}))

(t/defalias CharacterDataWrapper
  (t/HMap :mandatory {:code Integer :status String :copyright String
                      :attributionText String :attributionHTML String
                      :data CharacterDataContainer :etag String}
          :complete true))
