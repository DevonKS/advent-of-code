(ns advent-of-code-2020.day-23
  (:require [clojure.string :as string]))

(def cups [6 5 3 4 2 7 9 1 8])

(defn parse-cups
  [cups]
  (let [first-cup (first cups)
        cups-ll {:head first-cup}
        cup-pairs (partition 2 1 cups)
        cups-ll (reduce
                 (fn [cups-ll [c1 c2]]
                   (-> cups-ll
                       (assoc-in [c1 :next] c2)
                       (assoc-in [c2 :previous] c1)))
                 cups-ll
                 cup-pairs)
        last-cup (peek cups)
        cups-ll (-> cups-ll
                    (assoc-in [last-cup :next] first-cup)
                    (assoc-in [first-cup :previous] last-cup))]
    cups-ll))

(defn play-cups-game
  [cups game-length]
  (let [cup-values (keys (dissoc cups :head))
        max-cup (apply max cup-values)
        min-cup (apply min cup-values)
        [final-cups] (reduce
                      (fn [[cups cup-val] _]
                        (let [current-cup (get cups cup-val)
                              c1-val (:next current-cup)
                              c1 (get cups c1-val)
                              c2-val (:next c1)
                              c2 (get cups c2-val)
                              c3-val (:next c2)
                              c3 (get cups c3-val)
                              cs-set #{c1-val c2-val c3-val}
                              destination-cup-val (loop [dest-cup cup-val]
                                                    (let [new-dest-cup (dec dest-cup)
                                                          new-dest-cup (if (< new-dest-cup min-cup)
                                                                         max-cup
                                                                         new-dest-cup)]
                                                      (if-not (contains? cs-set new-dest-cup)
                                                        new-dest-cup
                                                        (recur new-dest-cup))))
                              destination-cup (get cups destination-cup-val)
                              cups (cond-> cups
                                     (not= cup-val (:next c3)) (assoc-in [cup-val :next] (:next c3))
                                     (not= cup-val (:next c3)) (assoc-in [(:next c3) :previous] cup-val)

                                     true (assoc-in [c3-val :next] (:next destination-cup))
                                     true (assoc-in [(:next destination-cup) :previous] c3-val)

                                     true (assoc-in [destination-cup-val :next] c1-val)
                                     true (assoc-in [c1-val :previous] destination-cup-val))]
                          [cups (get-in cups [cup-val :next])]))
                      [cups (:head cups)]
                      (range game-length))]
    final-cups))

(defn challenge-1
  [cups]
  (let [cups (parse-cups cups)
        final-cups (play-cups-game cups 100)
        solution (loop [current-val 1
                        solution []]
                   (let [next-val (get-in final-cups [current-val :next])]
                     (if (= 1 next-val)
                       solution
                       (recur next-val (conj solution next-val)))))]
    (string/join "" solution)))

(defn challenge-1!
  []
  (challenge-1 cups))

(defn challenge-2
  [cups]
  (let [next-cup (inc (apply max cups))
        cups (vec
              (concat
               cups
               (range next-cup (inc 1000000))))
        cups (parse-cups cups)
        final-cups (play-cups-game cups 10000000)
        c1-val (get-in final-cups [1 :next])
        c2-val (get-in final-cups [c1-val :next])]
    (* c1-val c2-val)))

(defn challenge-2!
  []
  (challenge-2 cups))
