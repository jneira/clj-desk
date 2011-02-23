(ns select
  (:use (clojure test)))

;;http://www.carlosble.com/2011/01/kata-de-las-apis-fluidas/
;;1. select("name").from(users)
;;2. select("name").from(users).where("age").greater_than(18)
;;3. select("name").from(users).where("surname").contains("rodriguez")
;;4. select("name").from(users).where("age").greater_than(18).and("location").is("san francisco")

;;(select [name] :from users)
;;(select [name] :from users [where :age greater-than 18])
;;(select [name] :from users [where :surname contains "rodriguez"])
;;(select [name] :from users [where :age greater_than 18 :and
;;                                :location is "san francisco])

(def users [{:name "juan" :age 23}
            {:name "pedro" :age 35}])

(def users-vec [["juan" 23] ["pedro" 35]])

(defn select
  ([keys _ coll]
     (vec (for [e coll]
            (vec (for [k keys] (e k))))))
  ([keys _ coll [{:keys}]]
     keys))


(deftest select-properties-of-elements-in-a-collection
  (testing "select a property"
    (is (= (select [:name] :from users)
           [["juan"] ["pedro"]])))
  (testing "select some properties"
    (is (= (select [:name :age] :from users)
           [["juan" 23] ["pedro" 35]])))
  (testing "maintain the order of keys"
    (is (= (select [:age :name] :from users)
           [[23 "juan"] [35 "pedro"]])))
  (testing "it works with vectors"
    (is (= (select [0 1] :from users-vec)
           [["juan" 23] ["pedro" 35]]))))

(def greater-than >)

(deftest filter-select-with-where-clause
  (testing "with one condition"
    (is (select [:name :age] :from users
                [:where :age greater-than 30])
        [["pedro 35"]])))

(run-tests)
