(ns advent-of-code-2020.day-22
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as cset]))

(defn parse-player
  [raw-player]
  (let [[player-string & deck] (string/split-lines raw-player)
        [_ player-id] (re-matches #"Player (\d+):" player-string)
        player-id (util/parse-int player-id)
        deck (mapv util/parse-int deck)]
    [player-id deck]))

(defn parse-input
  [input]
  (let [players (string/split input #"\n\n")]
    (reduce
     (fn [result raw-player]
       (let [[player-id deck] (parse-player raw-player)]
         (assoc result player-id deck)))
     {}
     players)))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 22)))

(defn play-combat
  [game-state]
  (loop [game-state game-state]
    (let [{p1-deck 1
           p2-deck 2} game-state
          p1-card (first p1-deck)
          p1-deck (vec (rest p1-deck))
          p2-card (first p2-deck)
          p2-deck (vec (rest p2-deck))
          winner (if (> p1-card p2-card) 1 2)
          new-game-state (-> game-state
                             (assoc 1 (if (= 1 winner)
                                        (conj p1-deck p1-card p2-card)
                                        (vec p1-deck)))
                             (assoc 2 (if (= 2 winner)
                                        (conj p2-deck p2-card p1-card)
                                        (vec p2-deck))))]
      (cond
        (empty? (get new-game-state 2)) [1 new-game-state]
        (empty? (get new-game-state 1)) [2 new-game-state]
        :else (recur new-game-state)))))

(defn play-recursive-combat
  [p1-deck p2-deck]
  (loop [previous-game-states #{}
         p1-deck p1-deck
         p2-deck p2-deck]
    ;(println previous-game-states)
    ;(println p1-deck)
    ;(println p2-deck)
    ;(println "----------------------------")
    (if (contains? previous-game-states [p1-deck p2-deck])
      [1 {1 p1-deck 2 p2-deck}]
      (let [p1-card (get p1-deck 0)
            new-p1-deck (subvec p1-deck 1)
            p2-card (get p2-deck 0)
            new-p2-deck (subvec p2-deck 1)
            [winner] (if (and (<= p1-card (count new-p1-deck))
                              (<= p2-card (count new-p2-deck)))
                       (play-recursive-combat (subvec new-p1-deck 0 p1-card)
                                              (subvec new-p2-deck 0 p2-card))
                       nil)
            winner (or winner (if (> p1-card p2-card) 1 2))
            new-p1-deck (if (= 1 winner)
                          (conj new-p1-deck p1-card p2-card)
                          new-p1-deck)
            new-p2-deck (if (= 2 winner)
                          (conj new-p2-deck p2-card p1-card)
                          new-p2-deck)]
        (cond
          (empty? new-p1-deck) [2 {1 new-p1-deck 2 new-p2-deck}]
          (empty? new-p2-deck) [1 {1 new-p1-deck 2 new-p2-deck}]
          :else (recur (conj previous-game-states [p1-deck p2-deck])
                       new-p1-deck
                       new-p2-deck))))))

#_(defn play-recursive-combat-no-escape
    [game-state]
    (loop [p1-deck (get game-state 1)
           p2-deck (get game-state 2)]
      (let [p1-card (get p1-deck 0)
            p1-deck (subvec p1-deck 1)
            p2-card (get p2-deck 0)
            p2-deck (subvec p2-deck 1)
            [winner] (if (and (<= p1-card (count p1-deck))
                              (<= p2-card (count p2-deck)))
                       (play-recursive-combat-no-escape {1 (subvec p1-deck 0 p1-card)
                                                         2 (subvec p2-deck 0 p2-card)})
                       nil)
            winner (or winner (if (> p1-card p2-card) 1 2))
            new-p1-deck (if (= 1 winner)
                          (conj p1-deck p1-card p2-card)
                          p1-deck)
            new-p2-deck (if (= 2 winner)
                          (conj p2-deck p2-card p1-card)
                          p2-deck)]
        (cond
          (empty? new-p1-deck) [2 {1 new-p1-deck 2 new-p2-deck}]
          (empty? new-p2-deck) [1 {1 new-p1-deck 2 new-p2-deck}]
          :else (recur new-p1-deck
                       new-p2-deck)))))

(defn count-winning-score
  [[winner game-state]]
  (let [winning-deck (get game-state winner)]
    (reduce
     (fn [result [card multiplier]]
       (+ result (* card multiplier)))
     0
     (map vector winning-deck (range (count winning-deck) 0 -1)))))

(def play-combat-and-count-winning-score
  (comp count-winning-score play-combat))

(def play-recursive-combat-and-count-winning-score
  (comp count-winning-score play-recursive-combat))

#_(def play-recursive-combat-no-escape-and-count-winning-score
    (comp count-winning-score play-recursive-combat-no-escape))

(defn challenge-1
  [game-state]
  (play-combat-and-count-winning-score game-state))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [game-state]
  (let [p1-deck (get game-state 1)
        p2-deck (get game-state 2)]
    (play-recursive-combat-and-count-winning-score p1-deck p2-deck)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
