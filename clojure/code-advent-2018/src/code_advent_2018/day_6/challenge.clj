(ns code-advent-2018.day-6.challenge
    (:require [clojure.string :as s]))

(def re #"(\d+), (\d+)")

(defn parse-line
    [line]
    (let [[x y] (rest (re-find re line))]
        [(Integer/parseInt x) (Integer/parseInt y)]))

(defn parse-file
    [filename]
    (map parse-line (s/split-lines (slurp (str "src/code_advent_2018/day_6/" filename)))))

(defn get-max-x
    [points]
    (apply max (map first points)))

(defn get-max-y
    [points]
    (apply max (map second points)))

(defn get-graph-points
    [max-x max-y]
    (for [x (range 0 (inc max-x))
          y (range 0 (inc max-y))]
       [x y]))

(defn manhatten-distance
    [[px py] [qx qy]]
    (+ (Math/abs (- px qx))
       (Math/abs (- py qy))))

(defn get-closest-point
    [graph-point points]
    (let [manhatten-distances (map #(vector (manhatten-distance graph-point %) %) points)
          lowest-distance (apply min-key first manhatten-distances)
          second-lowest-distance (apply min-key first (remove #(= % lowest-distance) manhatten-distances))]
       (when (not= (first lowest-distance) (first second-lowest-distance))
         (second lowest-distance))))

(defn get-point-areas
    [points]
    (let [max-x (get-max-x points)
          max-y (get-max-y points)
          graph-points (get-graph-points max-x max-y)]
        (reduce (fn 
                    [result graph-point] 
                    (let [closest-point (get-closest-point graph-point points)]
                       (if (nil? closest-point)
                           result
                           (assoc result closest-point (conj (or (get result closest-point) []) graph-point)))))
                {}
                graph-points)))

(defn infinite-area?
    [area max-x max-y]
    (some #(let [[x y] %]
              (or (zero? x)
                  (zero? y)
                  (>= x max-x)
                  (>= y max-y)))
          area))

(defn total-manhatten-ditance
    [points graph-point]
    (apply + (map #(manhatten-distance % graph-point) points)))

(defn challenge1
    [filename]
    (let [points (parse-file filename)
          max-x (get-max-x points)
          max-y (get-max-y points)
          point-areas (get-point-areas points)]
       (apply max (map count (filter #(not (infinite-area? % max-x max-y)) (vals point-areas))))))

(defn challenge2
    [filename max-total-man-dist]
    (let [points (parse-file filename)
          max-x (get-max-x points)
          max-y (get-max-y points)
          graph-points (get-graph-points max-x max-y)
          points-in-region (filter #(> max-total-man-dist (total-manhatten-ditance points %)) graph-points)]
      (count points-in-region)))