(ns advent-of-code-2020.day-9
  (:require [advent-of-code-2020.util :as util]
            [clojure.math.combinatorics :as combo]))

(defn parse-lines
  [lines]
  (mapv #(Long/parseLong %) lines))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 9)))

(defn is-num-valid?
  [preamble n]
  (let [preable-pairs (combo/combinations preamble 2)]
    (some #(= n (apply + %)) preable-pairs)))

(defn first-invalid-num
  [nums preamble-len]
  (loop [preamble (vec (take preamble-len nums))
         nums (vec (drop preamble-len nums))]
    (let [n (first nums)]
      (if-not (is-num-valid? preamble n)
        n
        (recur (conj (vec (rest preamble)) n)
               (vec (rest nums)))))))

(defn find-contigious-nums
  [nums n]
  (util/first-match
   #(= n (apply + %))
   (for [current-idx (range 0 (count nums))
         end-idx (range (+ 2 current-idx) (inc (count nums)))]
     (subvec nums current-idx end-idx))))

(defn challenge-1
  [nums preamble]
  (first-invalid-num nums preamble))

(defn challenge-1!
  []
  (challenge-1 (parse-input!) 25))

(defn challenge-2
  [nums preamble]
  (let [invalid-num (first-invalid-num nums preamble)
        contigious-nums (find-contigious-nums nums invalid-num)]
    (+ (apply min contigious-nums)
       (apply max contigious-nums))))

(defn challenge-2!
  []
  (challenge-2 (parse-input!) 25))
