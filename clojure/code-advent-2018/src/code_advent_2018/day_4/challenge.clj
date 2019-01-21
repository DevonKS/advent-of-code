(ns code-advent-2018.day-4.challenge
  (:require [java-time]
            [clojure.string :refer [split replace starts-with? split-lines]]))

(defn parse-line
  [line]
  (let [parts (split line #"\] ")
        time-string (replace (first parts) #"\[" "")
        time (java-time/local-date-time "yyyy-MM-dd HH:mm" time-string)
        action-string (second parts)
        action (cond
                 (starts-with? action-string "wakes") "wakes"
                 (starts-with? action-string "falls") "asleep"
                 :else "shift")
        guard-id (if (= action "shift") (Integer. (replace (second (split action-string #" ")) #"#" "")) -1)]
    {:time time :action action :guard-id guard-id}))

(defn challenge1
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_4/" filename))
        lines (map parse-line (split-lines file-string))
        sorted-lines (sort-by :time lines)
        guard-sleep-times (reduce (fn
                                    [result line]
                                    (if-let [shift? (= (:action line) "shift")]
                                      (assoc (assoc result :current-guard (:guard-id line)) :previous-time (:time line))
                                      (let [sleep? (= (:action line) "asleep")
                                            minutes-path [(:current-guard result) (if sleep? :awake :asleep)]
                                            minutes (or (get-in result minutes-path) [])
                                            new-minutes (range (.getMinute (:previous-time result)) (.getMinute (:time line)))]
                                        (assoc (assoc-in result minutes-path (apply conj minutes new-minutes)) :previous-time (:time line)))))
                                  {:current-guard -1 :previous-time -1}
                                  sorted-lines)
        most-asleep-guard-id (key (apply max-key #(count (:asleep (second %))) guard-sleep-times))
        most-asleep-minute-of-most-asleep-guard (first (last (sort-by second (frequencies (get-in guard-sleep-times [most-asleep-guard-id :asleep])))))]
    (* most-asleep-guard-id most-asleep-minute-of-most-asleep-guard)))

(defn challenge2
  [filename]
  (let [file-string (slurp (str "src/code_advent_2018/day_4/" filename))
        lines (map parse-line (split-lines file-string))
        sorted-lines (sort-by :time lines)
        guard-sleep-times (dissoc (reduce (fn
                                            [result line]
                                            (if-let [shift? (= (:action line) "shift")]
                                              (assoc (assoc result :current-guard (:guard-id line)) :previous-time (:time line))
                                              (let [sleep? (= (:action line) "asleep")
                                                    minutes-path [(:current-guard result) (if sleep? :awake :asleep)]
                                                    minutes (or (get-in result minutes-path) [])
                                                    new-minutes (range (.getMinute (:previous-time result)) (.getMinute (:time line)))]
                                                (assoc (assoc-in result minutes-path (apply conj minutes new-minutes)) :previous-time (:time line)))))
                                          {:current-guard -1 :previous-time -1}
                                          sorted-lines)
                                  :current-guard
                                  :previous-time)
        guards-most-slept-mins (map #(conj (last (sort-by second (frequencies (:asleep (second %))))) (first %)) guard-sleep-times)
        guard-most-slept-mins (last (sort-by second guards-most-slept-mins))]
    (* (first guard-most-slept-mins) (nth guard-most-slept-mins 2))))