(ns iterator
  (import (java.util Iterator
                     NoSuchElementException)))

(defn make-iterator [tree]
  (let [stack (ref [tree])]
    (proxy [Iterator] []
      (next []
            (if (seq @stack)
              (let [{:keys [left right value]}
                    (first @stack)]
                (dosync
                 (alter stack
                        #(concat (keep identity [left right])
                                 (rest %)))
                 value))
              (throw (NoSuchElementException.))))
      (hasNext [] (not (empty? @stack))))))

(def exp {:left {:left {:value 3}
                 :right {:right {:value 4} :value 1}
                 :value 6}
          :right {:left {:value 7} :value 8}
          :value 0})


