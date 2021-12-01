(ns advent-of-code-2020.day-25
  (:require [clojure.string :as string]
            [advent-of-code-2020.util :as util]))

(defn parse-input
  [input]
  (let [[card-pk-str door-pk-str] (string/split-lines input)
        card-pk (util/parse-int card-pk-str)
        door-pk (util/parse-int door-pk-str)]
    {:card-pk card-pk
     :door-pk door-pk}))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 25)))

(defn transform-subject-number
  [subject-n loops]
  (reduce (fn [value _]
            (rem (* value subject-n) 20201227))
          1
          (range loops)))

(defn reverse-engineer-loop-size
  [pk subject-n]
  (loop [loop-size 1
         value 1]
    (let [potential-pk (rem (* value subject-n) 20201227)]
      (if (= pk potential-pk)
        loop-size
        (recur (inc loop-size) potential-pk)))))

(defn determine-encryption-key
  [input subject-n]
  (let [card-loop-size (reverse-engineer-loop-size (:card-pk input) subject-n)
        encryption-key (transform-subject-number (:door-pk input) card-loop-size)]
    encryption-key))

(defn challenge-1
  [input]
  (determine-encryption-key input 7))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))
