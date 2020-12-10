(ns advent-of-code-2020.day-10
  (:require [advent-of-code-2020.util :as util]))

(defn parse-lines
  [lines]
  (mapv util/parse-int lines))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 10)))

(defn num-diffs
  [nums]
  (->> nums
       (partition 2 1)
       (mapv #(- (second %) (first %)))))

(defn prepare-nums
  [nums]
  (as-> nums xs
    (conj xs 0)
    (sort xs)
    (vec xs)
    (conj xs (+ 3 (peek xs)))))

(defn challenge-1
  [nums]
  (let [nums (prepare-nums nums)
        freqs (frequencies (num-diffs nums))]
    (* (get freqs 1) (get freqs 3))))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(def num-paths-to-end
  (memoize
   (fn [nums i]
     (if (== i (dec (count nums)))
       1
       (reduce
        (fn [sum j]
          (if (<= 0 (- (nth nums j) (nth nums i)) 3)
            (+ sum (num-paths-to-end nums j))
            sum))
        0
        (range (inc i) (count nums)))))))

(def num-paths-to-end-memo (memoize num-paths-to-end))

(defn challenge-2
  [nums]
  (let [nums (prepare-nums nums)]
    (num-paths-to-end-memo nums 0)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
