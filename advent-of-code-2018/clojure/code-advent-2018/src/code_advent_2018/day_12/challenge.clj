(ns code-advent-2018.day-12.challenge
  (:require [clojure.string :as s]))

(defn parse-file
  [filename]
  (let [lines (s/split-lines (slurp (str "src/code_advent_2018/day_12/" filename)))
        state-string (second (re-find #"initial state: ([.#]+)" (first lines)))
        state (into {} (map-indexed #(vector %1 (str %2)) state-string))
        generative-rules-lines (rest (rest lines))
        generative-rules (map #(let [[_ rule outcome] (re-find #"([.#]{5}) => ([.#]{1})" %)]
                                 [outcome rule])
                              generative-rules-lines)
        important-generative-rules (into #{} (map second (filter #(= "#" (first %)) generative-rules)))]
    [state important-generative-rules]))

(defn get-min-index
  [state]
  (apply min (map first state)))

(defn get-max-index
  [state]
  (apply max (map first state)))

(defn get-index-pots
  [index state]
  (apply str (map #(get state % ".") (range (- index 2) (+ index 3)))))

(defn do-generation
  [initial-state generative-rules num-iterations]
  (reduce (fn [state _]
            (let [min-index (get-min-index state)
                  max-index (get-max-index state)
                  index-range (range (- min-index 3) (+ max-index 4))]
              (reduce (fn [new-state index]
                        (if (contains? generative-rules (get-index-pots index state))
                          (assoc new-state index "#")
                          (assoc new-state index ".")))
                      state
                      index-range)))
          initial-state
          (range 0 num-iterations)))

(defn get-sum-of-state
  [state]
  (apply + (map first (filter #(= "#" (second %)) state))))

(defn challenge1
  [filename]
  (let [[initial-state generative-rules] (parse-file filename)]
    (get-sum-of-state (do-generation initial-state generative-rules 20))))

(defn challenge2
  [filename]
  (let [[initial-state generative-rules] (parse-file filename)
        state1000 (do-generation initial-state generative-rules 1000)
        state1001 (do-generation state1000 generative-rules 1)
        sumState1000 (get-sum-of-state state1000)]
    (+ sumState1000
       (* (- (get-sum-of-state state1001) sumState1000) (- 50000000000 1000)))))
