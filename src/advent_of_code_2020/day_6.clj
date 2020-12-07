(ns advent-of-code-2020.day-6
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn parse-group
  [group]
  (let [raw-people (string/split group #"\n")
        people (map set raw-people)]
    people))

(defn parse-input
  [input]
  (let [raw-groups (string/split input #"\n\n")
        groups (map parse-group raw-groups)]
    groups))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 6 1)))

(defn challenge-1
  [groups]
  (apply + (map #(count (reduce set/union %)) groups)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [groups]
  (apply + (map #(count (reduce set/intersection %)) groups)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
