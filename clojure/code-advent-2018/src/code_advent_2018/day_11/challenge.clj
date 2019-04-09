(ns code-advent-2018.day-11.challenge)

(defn get-hundreds-digit
  [num]
  (Integer/parseInt (str (nth (reverse (str num)) 2 \0))))

(defn find-power-level
  [serial-num [x y]]
  (-> (+ x 10)
      (* y)
      (+ serial-num)
      (* (+ x 10))
      (get-hundreds-digit)
      (- 5)))

(def find-power-level-memo (memoize find-power-level))

(defn get-total-power-level
  [serial-num [top-left-x top-left-y] grid-size]
  (apply + (for [x (range top-left-x (+ grid-size top-left-x))
                 y (range top-left-y (+ grid-size top-left-y))]
             (find-power-level-memo serial-num [x y]))))

(def get-total-power-level-memo
  (memoize (fn [serial-num [top-left-x top-left-y] grid-size]
             (if (<= grid-size 1)
               (find-power-level-memo serial-num [top-left-x top-left-y])
               (let [small-sqaure-power (get-total-power-level-memo serial-num [top-left-x top-left-y] (dec grid-size))
                     right-x (+ top-left-x (dec grid-size))
                     bottom-y (+ top-left-y (dec grid-size))
                     bottom-points (mapv #(vector % bottom-y) (range top-left-x (inc right-x)))
                     right-points (mapv #(vector right-x %) (range top-left-y (inc bottom-y)))
                     remaining-points (into #{} (concat bottom-points right-points))]
                 (apply + (conj (mapv #(find-power-level-memo serial-num %) remaining-points) small-sqaure-power)))))))

(defn get-highest-total-power-grid-point-memo
  [serial-num grid-size]
  (apply max-key
         #(second %)
         (for [x (range 1 (- 302 grid-size))
               y (range 1 (- 302 grid-size))]
           [[x y] (get-total-power-level-memo serial-num [x y] grid-size)])))

(defn get-highest-total-power-grid-point
  [serial-num grid-size]
  (apply max-key
         #(second %)
         (for [x (range 1 (- 302 grid-size))
               y (range 1 (- 302 grid-size))]
           [[x y] (get-total-power-level serial-num [x y] grid-size)])))

(defn challenge1
  [serial-num]
  (first (get-highest-total-power-grid-point-memo serial-num 3)))

(defn challenge2
  [serial-num]
  (first (apply max-key
                #(second %)
                (map #(let [[point power] (get-highest-total-power-grid-point-memo serial-num %)]
                        [(conj point %) power])
                     (range 1 301)))))

(defn create-summed-area-table
  [serial-num]
  (reduce (fn [summed-area-table [x y :as point]]
            (assoc summed-area-table point (+ (get summed-area-table [x (dec y)] 0)
                                              (get summed-area-table [(dec x) y] 0)
                                              (find-power-level-memo serial-num point)
                                              (- (get summed-area-table [(dec x) (dec y)] 0)))))
          {}
          (for [x (range 1 301)
                y (range 1 301)]
            [x y])))

(defn get-sum-of-rect
  [summed-area-table [top-left-x top-left-y :as top-left-point] square-size]
  (let [a [(dec top-left-x) (dec top-left-y)]
        b [(+ top-left-x (dec square-size)) (dec top-left-y)]
        c [(dec top-left-x) (+ top-left-y (dec square-size))]
        d [(+ top-left-x (dec square-size)) (+ top-left-y (dec square-size))]]
    (+ (get summed-area-table d 0)
       (get summed-area-table a 0)
       (- (get summed-area-table b 0))
       (- (get summed-area-table c 0)))))

(defn get-highest-total-power-grid-point-summed-area-table
  [summed-area-table square-size]
  (apply max-key
         #(second %)
         (for [x (range 1 (- 302 square-size))
               y (range 1 (- 302 square-size))]
           [[x y] (get-sum-of-rect summed-area-table [x y] square-size)])))

(defn challenge1-summed-area-table
  [serial-num]
  (first (get-highest-total-power-grid-point-summed-area-table (create-summed-area-table serial-num) 3)))

(defn challenge2-summed-area-table
  [serial-num]
  (let [summed-area-table (create-summed-area-table serial-num)]
    (first (apply max-key
                  #(second %)
                  (map #(let [[point power] (get-highest-total-power-grid-point-summed-area-table summed-area-table %)]
                          [(conj point %) power])
                       (range 1 301))))))