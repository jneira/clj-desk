(defprotocol TradeTax
  (trade-tax [trade logic]))

(defprotocol Commission
  (commission [trade logic]))

(defrecord Trade [ref-no account instrument principal tax-fees
                  unit-price quantity])

(defn calculate-tax [{:keys [principal]} logic]
  (logic principal))
(defn calculate-commision [{:keys [principal quantity]} logic]
  (logic principal quantity))

;;maybe in other(s) namespace(s)
(extend Trade
  TradeTax
  {:trade-tax calculate-tax}
  Commission
  {:commission calculate-commision})

(defn create-trade
  [{:keys [ref-no account instrument unit-price quantity]
    :as request}]
  (Trade. ref-no account instrument (* unit-price quantity) {}
          unit-price quantity))

;;0x3b a sample request 
(def request
     {:ref-no "trd-123"
     :account "nomura-123"
     :instrument "IBM"
     :unit-price 120
     :quantity 300})

(def my-trade (create-trade request))

(def my-trade-tax (trade-tax my-trade #(* % 0.05)))
(def my-commission
     (commission my-trade
                 (fn [p q] (* p (if (> q 100) 0.05 0.07)))))

(def my-trade-tax2 (calculate-tax {:principal 36000} #(* % 0.05)))
(def my-commission2
     (calculate-commision
      {:principal 36000 :quantity 300}
      (fn [p q] (* p (if (> q 100) 0.05 0.07)))))

(assert (= my-trade-tax my-trade-tax2))
(assert (= my-commission my-commission2))

(comment
   (defn trade
   "Make a trade from the request"
   [request]
   {:ref-no (:ref-no request)
   :account (:account request)
   :instrument (:instrument request)
   :principal (* (:unit-price request) (:quantity request))
   :tax-fees {}})

  (defn with-values [trade tax-fee value]
           (fn [request]
             (let [trdval (trade request)
                   principal (:principal trdval)]
               (assoc-in trdval [:tax-fees tax-fee]
                         (* principal (/ value 100))))))

;; from compojure source
         (defmacro redef
           "Redefine an existing value, keeping the metadata intact."
           [name value]
           `(let [m# (meta (var ~name))
                  v# (def ~name ~value)]
              (alter-meta! v# merge m#)
              v#)) 

; macro to decorate a function       
         (defmacro with-tax-fee
           "Wrap a function in one or more decorators"
           [func & decorators]
           `(redef ~func (-> ~func ~@decorators)))

         (with-tax-fee trade
           (with-values :tax 12)
           (with-values :commission 23)))
