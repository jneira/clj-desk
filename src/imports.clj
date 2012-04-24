
(do (ns imports)

    (for [x  ["clojure.repl"]]
      (refer (symbol x)))

    (in-ns 'citizen)
    (def x 1)
    (in-ns 'imports)
    (+ 2 citizen/x))

