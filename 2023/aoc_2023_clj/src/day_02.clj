(ns day-02
  (:require
   util
   [clojure.string :as str]))

(defn read-input
  [example]
  (util/parse-file
   2
   example
   (fn [l]
     (let [[_ id-str draws-str] (re-matches #"^Game (\d+): ([a-z0-9 ,;]+)$" l)
           draws (mapv
                  (fn [draw]
                    (reduce
                     (fn [acc d]
                       (let [[_ amount-str color] (re-matches #"^\s*(\d+) (red|green|blue)$" d)]
                         (case color
                           "red" (assoc acc :red (Integer/parseInt amount-str))
                           "green" (assoc acc :green (Integer/parseInt amount-str))
                           "blue" (assoc acc :blue (Integer/parseInt amount-str)))))
                     {}
                     (str/split draw #",")))
                  (str/split draws-str #";"))]
       {:id (Integer/parseInt id-str)
        :draws draws}))))

(read-input true)

(defn challenge-1
  [games]
  (let [bag {:red 12 :green 13 :blue 14}]
    (transduce
     (comp
      (filter (fn [g] (every?
                       (fn [d] (and (<= (get d :red 0) (:red bag))
                                    (<= (get d :green 0) (:green bag))
                                    (<= (get d :blue 0) (:blue bag))))
                       (:draws g))))
      (map :id))
     +
     games)))

(defn challenge-2
  [games]
  (transduce
   (comp
    (map (fn [g]
           (reduce
            (fn [acc d]
              (let [acc (if (> (get d :red 0) (get acc :red 0))
                          (assoc acc :red (:red d))
                          acc)
                    acc (if (> (get d :green 0) (get acc :green 0))
                          (assoc acc :green (:green d))
                          acc)
                    acc (if (> (get d :blue 0) (get acc :blue 0))
                          (assoc acc :blue (:blue d))
                          acc)]
                acc))
            {}
            (:draws g))))
    (map (fn [min-draw] (* (:red min-draw) (:green min-draw) (:blue min-draw)))))
   +
   games))

(defn run
  [example]
  (let [games (read-input example)]
    (println "Part 1:")
    (time (println (challenge-1 games)))
    (println)
    (println "Part 2:")
    (time (println (challenge-2 games)))))
