(ns assembly)

(defn parse-instr [i]
  (let [[w params] (.split #" " i)
        params (.split #"," params)]
    [(keyword  w)
     (map read-string params)]))

(defn read-source [file]
  (let [lines (.split  #"\r?\n" (slurp file))]
    (mapv parse-instr lines)))

(def ops
  {:push (fn [{:keys [stack sp]} val]
           {:stack (assoc stack sp val)
            :sp (inc sp)})})


(defn init-env [conf prog]
  {:prog prog
   :mem (vec (repeat 1024 0))
   :stack (vec (repeat 32 0))
   :ip 0 :sp 0
   :halt? false})

(defn exec-instr [env op & ps])

(defn exec-next-instr [{:keys [prog ip] :as env}]
  (let [[op & ps] (get prog ip)]
   (apply exec-instr env op ps)))

(defn halt? [env] )

(defn compile
  ([env]
     ())
  ([conf prog]
     (compile (init-env conf prog))))


