(ns advent-of-code-2020.day-11
  (:require [advent-of-code-2020.util :as util]))

(defn parse-line
  [line]
  (mapv #(case % \L :empty \# :occupied \. :floor) line))

(defn parse-input
  [lines]
  (mapv parse-line lines))

(defn parse-input!
  []
  (parse-input (util/challenge-file-lines! 11)))

(defn get-seat
  [seats [seat-idx row-idx]]
  (nth (nth seats row-idx nil) seat-idx nil))

(def in-seats
  (memoize
   (fn
     [[seat-idx row-idx] max-seat-idx max-row-idx]
     (and (>= row-idx 0)
          (>= seat-idx 0)
          (<= row-idx max-row-idx)
          (<= seat-idx max-seat-idx)))))

(defn find-first-seat-on-slope
  [seats seat-coords max-seat-idx max-row-idx slope]
  (loop [current-seat-coords (map + seat-coords slope)]
    (let [current-seat (get-seat seats current-seat-coords)]
      (cond
        (= :floor current-seat)
        (recur (map + current-seat-coords slope))

        (#{:occupied :empty} current-seat)
        current-seat-coords

        (not (in-seats current-seat-coords max-seat-idx max-row-idx))
        nil))))

(def get-adjacent-coords-memo
  (memoize
   (fn
     [[seat-idx row-idx] max-seat-idx max-row-idx]
     (for [c-row-idx [(dec row-idx) row-idx (inc row-idx)]
           c-seat-idx [(dec seat-idx) seat-idx (inc seat-idx)]
           :when (and (in-seats [c-seat-idx c-row-idx] max-seat-idx max-row-idx)
                      (not (and (= seat-idx c-seat-idx)
                                (= row-idx c-row-idx))))]
       [c-seat-idx c-row-idx]))))

(defn get-adjacent-coords
  [_ seat-coords max-seat-idx max-row-idx]
  (get-adjacent-coords-memo seat-coords max-seat-idx max-row-idx))

(defn get-adjacent-coords-2
  [seats seat-coords max-seat-idx max-row-idx]
  (into []
        (keep (fn [slope] (find-first-seat-on-slope seats seat-coords max-seat-idx max-row-idx slope)))
        [[-1 -1] [0 -1] [1 -1] [-1 0] [1 0] [-1 1] [0 1] [1 1]]))

(defn num-adjacent-occupied
  [seats seat-coords adj-coords-fns]
  (let [max-seat-idx (dec (count (first seats)))
        max-row-idx (dec (count seats))
        adjacent-coords (adj-coords-fns seats seat-coords max-seat-idx max-row-idx)]
    (util/count-if #(= :occupied (get-seat seats %)) adjacent-coords)))

(defn fill-seats
  [seats adj-coords-fn uncomfortable-num-adj-occupied]
  (into []
        (map-indexed
         (fn [row-idx row]
           (into []
                 (map-indexed
                  (fn [seat-idx seat]
                    (case seat
                      :occupied (if (>= (num-adjacent-occupied seats [seat-idx row-idx] adj-coords-fn)
                                        uncomfortable-num-adj-occupied)
                                  :empty
                                  :occupied)
                      :empty (if (zero? (num-adjacent-occupied seats [seat-idx row-idx] adj-coords-fn))
                               :occupied
                               :empty)
                      :floor :floor)))
                 row)))
        seats))

(defn num-occupied
  [seats]
  (apply + (map #(util/count-if (fn [seat] (= :occupied seat)) %) seats)))

(defn fill-seats-till-equilibrium
  [seats adj-coords-fn uncomfortable-num-adj-occupied]
  (loop [seats seats]
    (let [new-seats (fill-seats seats adj-coords-fn uncomfortable-num-adj-occupied)]
      (if (= seats new-seats)
        (num-occupied new-seats)
        (recur new-seats)))))

(defn challenge-1
  [seats]
  (fill-seats-till-equilibrium seats get-adjacent-coords 4))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [seats]
  (fill-seats-till-equilibrium seats get-adjacent-coords-2 5))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
