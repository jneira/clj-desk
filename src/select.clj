(ns select
  (:use (clojure test)))

;;http://www.carlosble.com/2011/01/kata-de-las-apis-fluidas/
;;1. select("name").from(users)
;;2. select("name").from(users).where("age").greater_than(18)
;;3. select("name").from(users).where("surname").contains("rodriguez")
;;4. select("name").from(users).where("age").greater_than(18).and("location").is("san francisco")

;;(select :name from users)
;;(select :name from users (where :age greater-than 18))
;;(select :name from users (where :surname contains "rodriguez))
;;(select :name from users (where :age greater_than 18 and
;;                                :location is "san francisco)

(deftest select-propiedad-from-container
  (def users [])
  (defn select [& body])
  (is (= (select :name :from users) ["juan pedro"])))

(run-tests)
