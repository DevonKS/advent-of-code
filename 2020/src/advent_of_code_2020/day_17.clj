(ns advent-of-code-2020.day-17
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]))

(defn parse-input
  [input]
  (let [lines (string/split-lines input)
        x-indices (range (count (first lines)))
        y-indices (range (count lines))
        parse-item (fn [item]
                     (case item
                       \. :inactive
                       \# :active))]
    (reduce
     (fn [result x]
       (reduce
        (fn [result y]
          (assoc result [x y 0] (parse-item (get-in lines [y x]))))
        result
        y-indices))
     {}
     x-indices)))

(defn parse-input-2
  [input]
  (let [lines (string/split-lines input)
        x-indices (range (count (first lines)))
        y-indices (range (count lines))
        parse-item (fn [item]
                     (case item
                       \. :inactive
                       \# :active))]
    (reduce
     (fn [result x]
       (reduce
        (fn [result y]
          (assoc result [x y 0 0] (parse-item (get-in lines [y x]))))
        result
        y-indices))
     {}
     x-indices)))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 17)))

(defn parse-input-2!
  []
  (parse-input-2 (util/read-challenge-file! 17)))

(defn get-neighbour-coords
  [[x y z]]
  (for [x-delta [-1 0 1]
        y-delta [-1 0 1]
        z-delta [-1 0 1]
        :when (not
               (and (zero? x-delta)
                    (zero? y-delta)
                    (zero? z-delta)))]
    [(+ x x-delta) (+ y y-delta) (+ z z-delta)]))

(defn get-neighbour-coords-2
  [[x y z w]]
  (for [x-delta [-1 0 1]
        y-delta [-1 0 1]
        z-delta [-1 0 1]
        w-delta [-1 0 1]
        :when (not
               (and (zero? x-delta)
                    (zero? y-delta)
                    (zero? z-delta)
                    (zero? w-delta)))]
    [(+ x x-delta) (+ y y-delta) (+ z z-delta) (+ w w-delta)]))

(defn cycle-cube
  [cubes get-neighbour-coords-fn result coord]
  (let [neighbour-coords (get-neighbour-coords-fn coord)
        num-active-neighbours (util/count-if
                               #(= :active (get cubes %))
                               neighbour-coords)
        state (get cubes coord :inactive)
        new-state (case state
                    :inactive (if (= 3 num-active-neighbours) :active :inactive)
                    :active (if (#{2 3} num-active-neighbours) :active :inactive))]
    (assoc result coord new-state)))

(defn print-cubes
  [cubes]
  (let [coords (keys cubes)
        x-vals (sort (set (map first coords)))
        y-vals (sort (set (map second coords)))
        z-vals (sort (set (map #(get % 2) coords)))
        state-str (fn [state]
                    (case state
                      :inactive \.
                      :active \#))]
    (println x-vals)
    (println y-vals)
    (reduce
     (fn [_ z]
       (println (str "z=" z))
       (reduce
        (fn [_ y]
          (reduce
           (fn [_ x]
             (print (state-str (get cubes [x y z] :inactive))))
           nil
           x-vals)
          (println ""))
        nil
        y-vals)
       (println ""))
     nil
     z-vals)))

(defn cycle-cubes
  [get-neighbour-coords-fn cubes]
  (let [current-coords (set (keys cubes))
        coords-to-cycle (reduce
                         (fn [result coords]
                           (into result (get-neighbour-coords-fn coords)))
                         current-coords
                         current-coords)]
    (reduce
     (partial cycle-cube cubes get-neighbour-coords-fn)
     cubes
     coords-to-cycle)))

(defn challenge-1
  [cubes]
  (let [cycled-cubes (reduce
                      (fn [result _]
                        (cycle-cubes get-neighbour-coords result))
                      cubes
                      (range 6))]
    (util/count-if (fn [[_ state]] (= :active state)) cycled-cubes)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [cubes]
  (let [cycled-cubes (reduce
                      (fn [result _]
                        (cycle-cubes get-neighbour-coords-2 result))
                      cubes
                      (range 6))]
    (util/count-if (fn [[_ state]] (= :active state)) cycled-cubes)))

(defn challenge-2!
  []
  (challenge-2 (parse-input-2!)))
