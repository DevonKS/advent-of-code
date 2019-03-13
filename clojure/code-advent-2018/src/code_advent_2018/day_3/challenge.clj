(ns code-advent-2018.day-3.challenge
  (:require [clojure.string :as s]))

(defn parse-claim
  [claim]
  (let [[_ id start-x start-y width height] (re-find #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)]
    {:id id
     :start-coordinates [(Integer/parseInt start-x) (Integer/parseInt start-y)]
     :dimensions [(Integer/parseInt width) (Integer/parseInt height)]}))

(defn parse-file
  [filename]
  (map parse-claim (s/split-lines (slurp (str "src/code_advent_2018/day_3/" filename)))))

(defn get-claim-points
  [claim]
  (let [[start-x start-y] (:start-coordinates claim)
        [width height] (:dimensions claim)
        x-points (range start-x (+ start-x width))
        y-points (range start-y (+ start-y height))]
    (reduce (fn [result x-point]
              (apply conj
                     result
                     (map (fn [y-point]
                            (vector x-point y-point))
                          y-points)))
            []
            x-points)))

(defn challenge1
  [filename]
  (let [claims (parse-file filename)
        claimed-points (reduce (fn [points claim]
                                 (apply conj points (get-claim-points claim)))
                               []
                               claims)
        frequencies-of-claimed-points (frequencies claimed-points)
        points-with->1-claims (count (filter #(>= (second %) 2) frequencies-of-claimed-points))]
    points-with->1-claims))

(defn challenge2
  [filename]
  (let [claims (parse-file filename)
        claimed-points (reduce (fn [points claim]
                                 (apply conj points (get-claim-points claim)))
                               []
                               claims)
        frequencies-of-claimed-points (frequencies claimed-points)]
    (->> claims
         (filter (fn [claim]
                   (every? #(= 1 (get frequencies-of-claimed-points %)) (get-claim-points claim))))
         first
         :id)))