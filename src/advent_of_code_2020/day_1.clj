(ns advent-of-code-2020.day-1
  (:require [advent-of-code-2020.util :as util]
            [clojure.math.combinatorics :as combo]))

(defn challenge-1
  [nums]
  (let [combinations (combo/combinations nums 2)
        significant-pair (first (filter #(= 2020 (apply + %)) combinations))]
    (apply * significant-pair)))

(defn challenge-1!
  []
  (let [nums (map #(Integer/parseInt %) (util/challenge-file-lines 1 1))]
    (challenge-1 nums)))

(defn challenge-2
  [nums]
  (let [combinations (combo/combinations nums 3)
        significant-pair (first (filter #(= 2020 (apply + %)) combinations))]
    (apply * significant-pair)))

(defn challenge-2!
  []
  (let [nums (map #(Integer/parseInt %) (util/challenge-file-lines 1 1))]
    (challenge-2 nums)))
