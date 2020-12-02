(ns advent-of-code-2020.day-2
  (:require [advent-of-code-2020.util :as util]))

(defn parse-line
  [line]
  (let [matches (re-matches #"(\d{1,2})-(\d{1,2}) (\w): (\w+)" line)
        lower (Integer/parseInt (nth matches 1))
        upper (Integer/parseInt (nth matches 2))
        character (first (nth matches 3))
        password (nth matches 4)]
    {:lower lower
     :upper upper
     :character character
     :password password}))

(defn parse-input!
  []
  (map parse-line (util/challenge-file-lines! 2 1)))

(defn is-password-valid-to-sled-rental-spec?
  [{:keys [lower upper character password]}]
  (let [num-chars (count (filter #(= character %) password))]
    (<= lower num-chars upper)))

(defn is-password-valid-to-toboggon-co-spec?
  [{:keys [lower upper character password]}]
  (let [[char1 char2] (map (comp #(= character %)
                                 (partial nth password)
                                 dec)
                           [lower upper])]
    (util/xor char1 char2)))

(defn count-valid
  [pred passwords]
  (count (filter pred passwords)))

(defn challenge-1
  [passwords]
  (count-valid is-password-valid-to-sled-rental-spec? passwords))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [passwords]
  (count-valid is-password-valid-to-toboggon-co-spec? passwords))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
