(ns id3tag
  [:use [ clojure.contrib.duck-streams
         :only (to-byte-array)]])

(def *mp3-file* "song.mp3")

(defn get-tags [file-name]
  (let [f (java.io.File. file-name)
        ba (to-byte-array f)
        from (- (count ba) 128)
        cad (String. ba from 127)
        not0 #(not= 0 (int %1))]
    (when (= (take 3 cad) '(\T\A\G))
      (vec (map (comp #(apply str %1)#(filter not0 %1))
                (partition 30 (drop 3 cad)))))))

(defn str-repeat
  "Returns a new String containing s repeated n times."
  [s n]
  (apply str (repeat n s)))

(defn lpad
  ([cad s n]
     (str (str-repeat s (- n (count cad))) cad))
  ([cad n] (lpad cad " " n)))

(defn rpad
  ([cad s n]
     (str cad (str-repeat s (- n (count cad)))))
  ([cad n] (rpad cad " " n)))

(defn pp-tags [& keys]
  (when (seq? keys)
    (let [init "ID3v1 Tags for "
          tags (get-tags *mp3-file*)
          entry #(str (lpad %1 15) " "
                      (rpad (tags %2) 37))]
      (dorun
       (map println
            (list (str init *mp3-file*)
                  (str-repeat "=" 52)
                  (when (some #{:song} keys)
                    (entry "Song:" 0)) 
                  (when (some #{:artist} keys)
                    (entry  "Artist:" 1))
                  (when (some #{:song} keys)
                    (entry "Album:" 2))
                  (when (some #{:year} keys)
                    (entry "Year:" 3))
                  (str-repeat "-" 52)))))))
