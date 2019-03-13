(ns code-advent-2018.day-1.challenge)

(defn parse-file
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_1/" filename))
        nums (map #(Integer/parseInt %) (clojure.string/split-lines file-string))]
    nums))

(defn challenge1
  [filename]
  (let [nums (parse-file filename)
        nums-sum (apply + nums)]
    nums-sum))

(defn challenge2
  [filename]
  (reduce (fn [info num]
            (let [current-freq (+ num (:current-freq info))]
              (if (contains? (:freq-seen info) current-freq)
                (reduced current-freq)
                {:freq-seen (conj (:freq-seen info) current-freq) :current-freq current-freq})))
          {:current-freq 0 :freq-seen #{}}
          (cycle (parse-file filename))))