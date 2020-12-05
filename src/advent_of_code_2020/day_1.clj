(ns advent-of-code-2020.day-1
  (:require [advent-of-code-2020.util :as util]
            [clojure.math.combinatorics :as combo]))

(defn parse-input!
  []
  (map util/parse-int (util/challenge-file-lines! 1 1)))

(defn challenge-1
  [nums]
  (let [combinations (combo/combinations nums 2)
        significant-pair (util/first-match #(= 2020 (apply + %)) combinations)]
    (apply * significant-pair)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [nums]
  (let [combinations (combo/combinations nums 3)
        significant-pair (util/first-match #(= 2020 (apply + %)) combinations)]
    (apply * significant-pair)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
