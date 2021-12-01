(ns code-advent-2018.day-9.challenge)

(defn get-current-player
    [num-players marble-num]
    (mod marble-num num-players))

(defn get-insert-index
    [current-marble len-board]
    (cond 
        (>= 1 len-board) 1
        (= len-board (+ 2 current-marble)) len-board
        :else (mod (+ 2 current-marble) len-board)))

(defn get-remove-index
    [current-marble len-board]
    (if (>= 1 len-board)
        0
        (let [remainder (rem (- current-marble 7) len-board)]
           (if (neg? remainder)
               (+ len-board remainder)
               remainder))))

(defn challenge1
    [num-players last-marble-value]
    (let [players-scores (into [] (repeat num-players 0))
          game (reduce (fn 
                          [result marble-num]
                        (if (zero? (mod marble-num 23))
                            (let [board (:board result)
                                  current-marble (:current-marble result)
                                  remove-index (get-remove-index current-marble (count board))
                                  marble-to-remove (get board remove-index)
                                  new-board (apply conj (subvec board 0 remove-index) (subvec board (inc remove-index)))
                                  current-player (get-current-player num-players marble-num)
                                  new-players-scores (update (:players-scores result) current-player #(+ % marble-to-remove marble-num))]
                                {:players-scores new-players-scores
                                 :board new-board
                                 :current-marble remove-index})
                            (let [board (:board result)
                                  insert-index (get-insert-index (:current-marble result) (count (:board result)))
                                  new-board (apply conj (subvec board 0 insert-index) marble-num (subvec board insert-index))]
                                (assoc (assoc result :current-marble insert-index) :board new-board))))
                       {:players-scores players-scores
                        :board [0]
                        :current-marble 0}
                       (range 1 (inc last-marble-value)))]
        (apply max (:players-scores game))))

(defn get-marble-to-remove
    [marble-circle current-marble-id]
    (loop [x 0
           current-marble (get marble-circle current-marble-id)
           current-marble-id current-marble-id]
        (if (>= x 7)
            [current-marble-id current-marble]
            (let [new-x (inc x)
                  new-current-marble-id (:previous current-marble)
                  new-current-marble (get marble-circle new-current-marble-id)]
                (recur new-x new-current-marble new-current-marble-id)))))

(defn remove-marble
    [marble-circle marble-id]
    (let [marble-to-remove (get marble-circle marble-id)
          previous-marble-id (:previous marble-to-remove)
          previous-marble (get marble-circle previous-marble-id)
          next-marble-id (:next marble-to-remove)
          next-marble (get marble-circle next-marble-id)]
        (merge (dissoc marble-circle marble-id)
               {previous-marble-id (assoc previous-marble :next next-marble-id)
                next-marble-id (assoc next-marble :previous previous-marble-id)})))

(defn insert-marble
    [marble-circle marble-num current-marble-id]
    (let [current-marble (get marble-circle current-marble-id)
          next-marble-id (:next current-marble)
          next-marble (get marble-circle next-marble-id)
          next-next-marble-id (:next next-marble)
          next-next-marble (get marble-circle next-next-marble-id)]
        (merge marble-circle {next-marble-id (assoc next-marble :next marble-num) 
                              next-next-marble-id (assoc next-next-marble :previous marble-num) 
                              marble-num {:value marble-num :previous next-marble-id :next next-next-marble-id}})))

(defn challenge2
    [num-players last-marble-value]
    (let [players-scores (into [] (repeat num-players 0))
          game (reduce (fn
                          [result marble-num]
                          (if (zero? (mod marble-num 23))
                              (let [marble-circle (:marble-circle result)
                                    current-marble (:current-marble result)
                                    [marble-to-remove-id marble-to-remove] (get-marble-to-remove marble-circle current-marble)
                                    new-marble-circle (remove-marble marble-circle marble-to-remove-id)
                                    current-player (get-current-player num-players marble-num)
                                    new-players-scores (update (:players-scores result) current-player #(+ % (:value marble-to-remove) marble-num))]
                                {:players-scores new-players-scores
                                 :marble-circle new-marble-circle
                                 :current-marble (:next marble-to-remove)})
                              (let [marble-circle (:marble-circle result)
                                    new-marble-circle (insert-marble marble-circle marble-num (:current-marble result))]
                                {:players-scores (:players-scores result)
                                 :marble-circle new-marble-circle
                                 :current-marble marble-num})))
                       {:players-scores players-scores
                        :marble-circle {0 {:value 0 :previous 1 :next 1} 1 {:value 1 :previous 0 :next 0}}
                        :current-marble 1}
                       (range 2 (inc last-marble-value)))]
        (apply max (:players-scores game))))