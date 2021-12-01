(ns advent-of-code-2020.day-12
  (:require [advent-of-code-2020.util :as util]))

(defn parse-line
  [line]
  (let [[_ op n] (re-matches #"([NSEWLRF]{1})(\d+)" line)]
    {:op-code (keyword op)
     :n (util/parse-int n)}))

(defn parse-lines
  [lines]
  (mapv parse-line lines))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 12)))

(defn move-boat
  [dir [x y] quantity]
  (case dir
    :N [x (+ y quantity)]
    :S [x (- y quantity)]
    :E [(+ x quantity) y]
    :W [(- x quantity) y]))

(defn dir->angle
  [dir]
  (case dir :N 0 :E 90 :S 180 :W 270))

(defn angle->dir
  [angle]
  (case angle 0 :N 90 :E 180 :S 270 :W))

(defn rotate-boat
  [current-dir rotate-dir angle]
  (let [angle (mod angle 360)
        current-angle (dir->angle current-dir)
        rotate-op (if (= :L rotate-dir) - +)
        new-angle (mod (rotate-op current-angle angle) 360)]
    (angle->dir new-angle)))

(defn perform-ops
  [ops]
  (reduce
   (fn [nav-info op]
     (case (:op-code op)
       (:N :S :E :W) (update nav-info :current-pos (fn [pos] (move-boat (:op-code op) pos (:n op))))
       (:L :R) (update nav-info :direction (fn [current-dir] (rotate-boat current-dir (:op-code op) (:n op))))
       :F (update nav-info :current-pos (fn [pos] (move-boat (:direction nav-info) pos (:n op))))))
   {:current-pos [0 0]
    :direction :E}
   ops))

(defn rotate-point
  [[x y] rotate-dir angle]
  (let [angle (mod angle 360)
        angle (if (= :L rotate-dir)
                angle
                (- angle))
        radians (Math/toRadians angle)]
    [(- (* x (int (Math/cos radians)))
        (* y (int (Math/sin radians))))
     (+ (* x (int (Math/sin radians)))
        (* y (int (Math/cos radians))))]))

(defn move-boat-to-waypoint
  [[w-x w-y] [c-x c-y] quantity]
  [(+ c-x (* w-x quantity))
   (+ c-y (* w-y quantity))])

(defn perform-ops-2
  [ops]
  (reduce
   (fn [nav-info op]
     (case (:op-code op)
       (:N :S :E :W) (update nav-info :waypoint (fn [pos] (move-boat (:op-code op) pos (:n op))))
       (:L :R) (update nav-info :waypoint (fn [waypoint] (rotate-point waypoint (:op-code op) (:n op))))
       :F (update nav-info :current-pos (fn [pos] (move-boat-to-waypoint (:waypoint nav-info) pos (:n op))))))
   {:current-pos [0 0]
    :waypoint [10 1]}
   ops))

(defn manhattan-distance
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn challenge-1
  [ops]
  (let [{:keys [current-pos]} (perform-ops ops)]
    (manhattan-distance current-pos)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [ops]
  (let [{:keys [current-pos]} (perform-ops-2 ops)]
    (manhattan-distance current-pos)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
