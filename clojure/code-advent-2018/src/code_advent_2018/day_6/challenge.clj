(ns code-advent-2018.day-6.challenge
  (:require [clojure.string :as s]
            [clojure.core.reducers :as r]))

(defn parse-line
  [line]
  (let [[_ x y] (re-find #"(\d+), (\d+)" line)]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn parse-file
  [filename]
  (map parse-line (s/split-lines (slurp (str "src/code_advent_2018/day_6/" filename)))))

(defn get-max-x
  [points]
  (apply max (map first points)))

(defn get-min-x
  [points]
  (apply min (map first points)))

(defn get-max-y
  [points]
  (apply max (map second points)))

(defn get-min-y
  [points]
  (apply min (map second points)))

(defn get-graph-points
  [max-x min-x max-y min-y]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
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
        min-x (get-min-x points)
        max-y (get-max-y points)
        min-y (get-min-y points)
        graph-points (get-graph-points max-x min-x max-y min-y)]
    (reduce (fn
              [result graph-point]
              (let [closest-point (get-closest-point graph-point points)]
                (if (nil? closest-point)
                  result
                  (assoc result closest-point (conj (or (get result closest-point) []) graph-point)))))
            {}
            graph-points)))

(defn get-point-area
  [points point-areas point]
  (let [closest-point (get-closest-point point points)]
    (if (nil? closest-point)
      point-areas
      (assoc point-areas closest-point (conj (get point-areas closest-point []) point)))))

(defn merge-point-areas
  ([] {})
  ([& m] (apply merge-with into m)))

(defn get-point-areas-parallel
  [points]
  (r/fold merge-point-areas
          (fn ([] {}) ([point-areas point] (get-point-area points point-areas point)))
          (into [] (get-graph-points (get-max-x points)
                            (get-min-x points)
                            (get-max-y points)
                            (get-min-y points)))))

(defn infinite-area?
  [area max-x min-x max-y min-y]
  (some #(let [[x y] %]
           (or (<= x min-x)
               (<= y min-y)
               (>= x max-x)
               (>= y max-y)))
        area))

(defn total-manhatten-distance
  [points graph-point]
  (apply + (map #(manhatten-distance % graph-point) points)))

(defn challenge1
  [filename]
  (let [points (parse-file filename)
        max-x (get-max-x points)
        min-x (get-min-x points)
        max-y (get-max-y points)
        min-y (get-min-y points)
        point-areas (get-point-areas points)]
    (apply max (map count (filter #(not (infinite-area? % max-x min-x max-y min-y)) (vals point-areas))))))

(defn challenge1-parallel
  [filename]
  (let [points (parse-file filename)
        max-x (get-max-x points)
        min-x (get-min-x points)
        max-y (get-max-y points)
        min-y (get-min-y points)
        point-areas (get-point-areas-parallel points)]
    (apply max (map count (filter #(not (infinite-area? % max-x min-x max-y min-y)) (vals point-areas))))))

(defn challenge2
  [filename max-total-man-dist]
  (let [points (parse-file filename)
        max-x (get-max-x points)
        min-x (get-min-x points)
        max-y (get-max-y points)
        min-y (get-min-y points)
        graph-points (get-graph-points max-x min-x max-y min-y)
        points-in-region (filter #(> max-total-man-dist (total-manhatten-distance points %)) graph-points)]
    (count points-in-region)))