(ns day-01
  (:require
   util
   [aho.core :as aho]))

(defn challenge-1
  [lines]
  (let [parsed-lines (map (fn [l] (->> l
                                       (filter #(Character/isDigit %))
                                       (map #(Character/digit % 10))))
                          lines)
        nums (map (fn [cs] (if (empty? cs)
                             0
                             (+ (* (first cs) 10) (last cs))))
                  parsed-lines)]
    (reduce + nums)))

(defn challenge-2
  [lines]
  (let [match->num {:one 1
                    :two 2
                    :three 3
                    :four 4
                    :five 5
                    :six 6
                    :seven 7
                    :eight 8
                    :nine 9
                    :1 1
                    :2 2
                    :3 3
                    :4 4
                    :5 5
                    :6 6
                    :7 7
                    :8 8
                    :9 9}
        graph (aho/build-automaton [[:one "one"]
                                    [:two "two"]
                                    [:three "three"]
                                    [:four "four"]
                                    [:five "five"]
                                    [:six "six"]
                                    [:seven "seven"]
                                    [:eight "eight"]
                                    [:nine "nine"]
                                    [:1 "1"]
                                    [:2 "2"]
                                    [:3 "3"]
                                    [:4 "4"]
                                    [:5 "5"]
                                    [:6 "6"]
                                    [:7 "7"]
                                    [:8 "8"]
                                    [:9 "9"]])
        parsed-lines (map (fn [l]
                            (let [matches (aho/search graph l)
                                  parsed-matches (map (fn [{:keys [pattern]}] (get match->num pattern)) matches)]
                              parsed-matches))
                          lines)
        nums (map (fn [cs] (if (empty? cs)
                             0
                             (+ (* (first cs) 10) (last cs))))
                  parsed-lines)]
    (reduce + nums)))

(defn run
  [example]
  (let [lines (util/parse-file 1 example identity)]
    (println "Part 1:")
    (time (println (challenge-1 lines)))
    (println)
    (println "Part 2:")
    (time (println (challenge-2 lines)))))
