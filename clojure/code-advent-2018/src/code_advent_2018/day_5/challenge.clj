(ns code-advent-2018.day-5.challenge
  (:require [clojure.string :as str]))

(defn react-string
  [s]
  (reduce (fn [result char]
            (let [result-string (str result)
                  s1 (str (last result-string))
                  s1-lower (str/lower-case s1)
                  s2 (str char)
                  s2-lower (str/lower-case s2)]
              (if (and (= s1-lower s2-lower) (not= s1 s2))
                (subs result-string 0 (- (count result-string) 1))
                (str result s2))))
          s))

(defn challenge1
  [filename]
  (let [file-string (str/trim (slurp (str "src/code_advent_2018/day_5/" filename)))]
    (count (react-string file-string))))

(defn challenge2
  [filename]
  (let [file-string (str/trim (slurp (str "src/code_advent_2018/day_5/" filename)))
        alphabet (map char (range (int \a) (inc (int \z))))
        polymers (map #(str/replace (str/replace file-string (re-pattern (str/upper-case %)) "") (re-pattern (str %)) "") alphabet)
        reacted-polymers (map react-string polymers)]
    (apply min (map count reacted-polymers))))