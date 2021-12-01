(ns advent-of-code-2020.day-24
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]))

(defn parse-instructions
  [raw-instructions]
  (loop [raw-instructions (seq raw-instructions)
         instructions []]
    (if (empty? raw-instructions)
      instructions
      (let [first-char (first raw-instructions)
            instruction-len (if (#{\e \w} first-char) 1 2)
            [instruction raw-instructions] (split-at instruction-len raw-instructions)
            instruction (keyword (string/join "" instruction))]
        (recur raw-instructions (conj instructions instruction))))))

(defn parse-input
  [input]
  (mapv parse-instructions input))

(defn parse-input!
  []
  (parse-input (util/challenge-file-lines! 24)))

(defn find-final-point
  [instructions]
  (reduce
   (fn [pt instruction]
     (let [delta (case instruction
                   :e [1 -1 0]
                   :w [-1 1 0]
                   :nw [0 1 -1]
                   :ne [1 0 -1]
                   :sw [-1 0 1]
                   :se [0 -1 1])]
       (mapv + pt delta)))
   [0 0 0]
   instructions))

(defn follow-instructions
  [instructions]
  (reduce
   (fn [acc instructions]
     (let [final-point (find-final-point instructions)
           point-val (get acc final-point 0)
           new-point-val (if (zero? point-val) 1 0)]
       (if (= 1 new-point-val)
         (assoc acc final-point new-point-val)
         (dissoc acc final-point))))
   {}
   instructions))

(def get-tile-neighbours
  (memoize
   (fn
     [pt]
     (mapv #(mapv + pt %)
           [[1 -1 0]
            [-1 1 0]
            [0 1 -1]
            [1 0 -1]
            [-1 0 1]
            [0 -1 1]]))))

(defn flip-tile
  [tiles pt]
  (let [tile (get tiles pt 0)
        neighbours (get-tile-neighbours pt)
        num-1-neighbours (count (into []
                                      (comp
                                       (map #(get tiles % 0))
                                       (filter #(= 1 %)))
                                      neighbours))]
    (cond
      (and (= 0 tile)
           (= 2 num-1-neighbours))
      1

      (and (= 1 tile)
           (or (zero? num-1-neighbours)
               (> num-1-neighbours 2)))
      0

      :else
      tile)))

(defn flip-tiles
  [tiles]
  (let [all-pts (keys tiles)
        all-neighbours (into #{}
                             (mapcat #(get-tile-neighbours %))
                             all-pts)]
    (reduce
     (fn [acc pt]
       (let [new-col (flip-tile tiles pt)]
         (if (= 1 new-col)
           (assoc acc pt new-col)
           acc)))
     {}
     all-neighbours)))

(defn challenge-1
  [instructions]
  (let [final-tiles (follow-instructions instructions)]
    (count final-tiles)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [instructions]
  (let [initial-state (follow-instructions instructions)
        final-state (reduce (fn [state _] (flip-tiles state))
                            initial-state
                            (range 100))]
    (count final-state)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
