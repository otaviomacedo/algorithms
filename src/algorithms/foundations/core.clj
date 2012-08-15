(ns algorithms.foundations.core
  (:use [clojure.contrib.string :only (split)])
  (:import (java.io BufferedReader FileReader)))

(defn less-than? [a b]
  (neg? (compare a b)))

(defn greater-than? [a b]
  (pos? (compare a b)))

(defn bin-search [element collection]
  (loop [start 0
         end (dec (count collection))]
    (when-not (> start end)
      (let [half (int (/ (+ start end) 2))
            pivot (nth collection half)]
        (cond
          (greater-than? element pivot) (recur (inc half) end)
          (less-than? element pivot) (recur start (dec half))
          :else half)))))

(defn find-insertion-place [element collection]
  (loop [start 0
         end (dec (count collection))]
    (if (<= start end)
      (let [half (int (/ (+ start end) 2))
            pivot (nth collection half)]
        (cond
          (pos? (compare element pivot)) (recur (inc half) end)
          (neg? (compare element pivot)) (recur start (dec half))
          :else half))
      end)))

(defn insert-into-binary [element coll]
  (let [h (inc (find-insertion-place element coll))]
    (concat (take h coll) [element] (drop h coll))))

(defn insert-into
  ([element list] (insert-into element [] list))
  ([element less greater]
    (if (or (empty? greater) (<= element (first greater)))
      (concat less [element] greater)
      (recur element (conj less (first greater)) (vec (rest greater))))))

(defn insertion-sort
  ([list] (insertion-sort [] list))
  ([sorted unsorted]
    (if (empty? unsorted)
      sorted
      (recur (insert-into-binary (first unsorted) sorted) (rest unsorted)))))

(defn selection-sort
  ([list] (selection-sort [] list))
  ([sorted unsorted]
    (if (empty? unsorted)
      sorted
      (let [min-element (apply min unsorted)]
        (recur (conj sorted min-element) (remove #{min-element} unsorted))))))

(defn linear-search
  ([element list] (linear-search element list 0))
  ([element list i]
    (when-not (empty? list)
      (if (= element (first list))
        i
        (recur element (rest list) (inc i))))))

(defn find-sum-pair-sorted
  "Given a collection of n integers and another integer x, determines whether or not there
   exist two elements in coll whose sum is exactly x."
  [x coll]
  (when-not (empty? coll)
    (let [candidate (first coll)
          difference (- x candidate)
          rest (rest coll)]
      (if (bin-search difference rest)
          #{candidate difference}
        (recur x rest)))))

(defn find-sum-pair [x coll]
  (find-sum-pair-sorted x (insertion-sort coll)))



