(ns advent-of-code-2020.day-5
  (:require [advent-of-code-2020.util :as util]
            [clojure.set :as set]))

(defn binary-string->number
  [s char-0 char-1]
  (as-> s result
    (map #(cond (= % char-0) 0 (= % char-1) 1) result)
    (apply str result)
    (Integer/parseInt result 2)))

(defn row-string->row-number
  [row-string]
  (binary-string->number row-string \F \B))

(defn seat-string->seat-number
  [seat-string]
  (binary-string->number seat-string \L \R))

(defn seat-id
  [row-number seat-number]
  (+ seat-number (* 8 row-number)))

(defn parse-line
  [line]
  (let [row-string (apply str (take 7 line))
        row-number (row-string->row-number row-string)
        seat-string (apply str (take 3 (drop 7 line)))
        seat-number (seat-string->seat-number seat-string)]
    {:row row-number
     :seat seat-number
     :seat-id (seat-id row-number seat-number)}))

(defn parse-input
  [lines]
  (map parse-line lines))

(defn parse-input!
  []
  (parse-input (util/challenge-file-lines! 5)))

(defn challenge-1
  [seats]
  (apply max (map :seat-id seats)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [seats]
  (let [taken-seat-ids (set (map :seat-id seats))
        max-taken-seat-id (apply max taken-seat-ids)
        all-free-seat-ids (set (range 0 max-taken-seat-id))
        free-seat-ids (set/difference all-free-seat-ids taken-seat-ids)
        free-seat-id (util/first-match
                      #(and (contains? taken-seat-ids (dec %))
                            (contains? taken-seat-ids (inc %)))
                      free-seat-ids)]
    free-seat-id))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
