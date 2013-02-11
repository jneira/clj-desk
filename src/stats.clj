(ns stats
  (use [incanter core stats]))

(mahalanobis-distance (matrix [[1 5 4] [9 7 3] [11 13 17]]))

;; (1.3693063937629155 1.2583057392117918 1.4288690166235207)

(do (use '(incanter core stats))
    (mahalanobis-distance (matrix [[1 5 4] [9 7 3] [11 13 17]])) )
