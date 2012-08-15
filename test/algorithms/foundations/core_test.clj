(ns algorithms.foundations.core-test
  (:use expectations)
  (:use algorithms.foundations.core))

(def input [88, 80, 65, 59, 54, 30, 27, 26, 25, 24, 0, -1, -9, -777])

(expect '(-777 -9 -1 0 24 25 26 27 30 54 59 65 80 88) (insertion-sort input))
(expect '(-777 -9 -1 0 24 25 26 27 30 54 59 65 80 88) (selection-sort input))
(expect #{54 24} (find-sum-pair-sorted 78 input))
(expect nil (find-sum-pair -12 input))

