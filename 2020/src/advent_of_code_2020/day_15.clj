(ns advent-of-code-2020.day-15)

(defn play-memory-game
  [starting-numbers end]
  (loop [previous-indices (zipmap (pop starting-numbers) (range))
         current-num (peek starting-numbers)
         current-index (dec (count starting-numbers))]
    (if (== end (inc current-index))
      current-num
      (let [next-num (if (contains? previous-indices current-num)
                       (- current-index (get previous-indices current-num))
                       0)
            previous-indices (assoc previous-indices current-num current-index)]
        (recur previous-indices next-num (inc current-index))))))

(defn challenge-1
  [nums]
  (play-memory-game nums 2020))

(defn challenge-1!
  []
  (challenge-1 [11 0 1 10 5 19]))

(defn challenge-2
  [nums]
  (play-memory-game nums 30000000))

(defn challenge-2!
  []
  (challenge-2 [11 0 1 10 5 19]))
