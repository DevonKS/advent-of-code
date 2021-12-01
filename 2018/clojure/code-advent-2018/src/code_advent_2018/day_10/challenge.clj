(ns code-advent-2018.day-10.challenge
  (:require [clojure.string :as s]))

(defn parse-line
  [line]
  (let [[_ start-x start-y velocity-x velocity-y] (re-find #"position=< *(-?\d+), +(-?\d+)> velocity=< *(-?\d+), +(-?\d+)>" line)]
    {:current-pos [(Integer/parseInt start-x) (Integer/parseInt start-y)]
     :velocity [(Integer/parseInt velocity-x) (Integer/parseInt velocity-y)]}))

(defn parse-file
  [filename]
  (mapv parse-line (s/split-lines (slurp (str "src/code_advent_2018/day_10/" filename)))))

(defn apply-velocity
  [[x y] [vx vy]]
  [(+ x vx) (+ y vy)])

(defn apply-velocities
  [points]
  (mapv #(update % :current-pos (fn [current-pos] (apply-velocity current-pos (:velocity %)))) points))

(defn get-max-x
  [points]
  (first (apply max-key first points)))

(defn get-min-x
  [points]
  (first (apply min-key first points)))

(defn get-max-y
  [points]
  (second (apply max-key second points)))

(defn get-min-y
  [points]
  (second (apply min-key second points)))

(defn get-area
  [points]
  (* (- (get-max-x points) (get-min-x points))
     (- (get-max-y points) (get-min-y points))))

(defn get-board-string
  [points]
  (let [min-x (get-min-x points)
        max-x (get-max-x points)
        min-y (get-min-y points)
        max-y (get-max-y points)]
    (s/join "\n"
            (reduce (fn [board-string y]
                      (conj board-string (apply str (map #(if (contains? points [% y]) "#" ".") (range min-x (inc max-x))))))
                    []
                    (range min-y (inc max-y))))))

(defn get-points
  [points-velocity]
  (mapv #(:current-pos %) points-velocity))

(defn challenge1
  [filename]
  (loop [points-velocity (parse-file filename)
         next-points-velocity (apply-velocities points-velocity)]
    (let [points (get-points points-velocity)]
      (if (> (get-area (get-points next-points-velocity)) (get-area points))
        (get-board-string (into #{} points))
        (recur next-points-velocity (apply-velocities next-points-velocity))))))

(defn challenge2
  [filename]
  (loop [points-velocity (parse-file filename)
         next-points-velocity (apply-velocities points-velocity)
         seconds 0]
    (if (> (get-area (get-points next-points-velocity))
           (get-area (get-points points-velocity)))
      seconds
      (recur next-points-velocity (apply-velocities next-points-velocity) (inc seconds)))))