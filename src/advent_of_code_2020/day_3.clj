(ns advent-of-code-2020.day-3
  (:require [advent-of-code-2020.util :as util]))

(defn parse-lines
  [lines]
  (mapv (fn [line]
          (mapv (fn [c]
                  (case c
                    \. false
                    \# true))
                line))
        lines))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 3)))

(defn get-in-grid
  [grid max-x [x y]]
  (get-in grid [y (mod x max-x)]))

(defn num-trees
  [grid slope]
  (let [start [0 0]
        max-x (count (first grid))
        max-y (count grid)
        points (loop [points []
                      current-point start]
                 (if (< (+ (second slope)
                           (second current-point))
                        max-y)
                   (let [new-point (mapv + current-point slope)]
                     (recur (conj points new-point) new-point))
                   points))
        tree-count (util/count-if (partial get-in-grid grid max-x) points)]
    tree-count))

(defn challenge-1
  [grid]
  (num-trees grid [3 1]))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [grid]
  (let [slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]
        num-trees-product (apply * (map (partial num-trees grid) slopes))]
    num-trees-product))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
