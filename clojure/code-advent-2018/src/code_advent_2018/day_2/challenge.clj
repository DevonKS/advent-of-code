(ns code-advent-2018.day-2.challenge
  (:require [clojure.math.numeric-tower :refer [abs]]))

(defn challenge1
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_2/" filename))
        box-ids (clojure.string/split-lines file-string)
        num-2s&3s (reduce (fn
                            [info box-id]
                            (let [char-frequencies (set (vals (frequencies box-id)))
                                  twos? (contains? char-frequencies 2)
                                  threes? (contains? char-frequencies 3)
                                  new#twos (+ (:#twos info) (if twos? 1 0))
                                  new#threes (+ (:#threes info) (if threes? 1 0))]
                              {:#twos new#twos :#threes new#threes}))
                          {:#twos 0 :#threes 0}
                          box-ids)]
    (* (:#twos num-2s&3s) (:#threes num-2s&3s))))

(defn test-string-for-likeness
  [s strings-to-check]
  (first (filter
          #(= 1 (abs (- (count s) (count %))))
          (map
           (fn
             [string-to-check]
             (clojure.string/join "" (map #(if (= %1 %2) %1 "") s string-to-check)))
           strings-to-check))))

(defn challenge2
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_2/" filename))
        box-ids (clojure.string/split-lines file-string)]
    (first (filter
            (comp not clojure.string/blank?)
            (map
             #(test-string-for-likeness % box-ids)
             box-ids)))))