(ns code-advent-2018.day-14.challenge)

(defn num->digits
  [num]
  (mapv #(Integer/parseInt (str %)) (str num)))

(defn get-next-recipes
  [recipes elves]
  (let [next-recipes-num (apply + (map #(:current-val %) elves))
        next-recipes (num->digits next-recipes-num)]
    (apply conj recipes next-recipes)))

(defn get-next-recipes2
  [recipes elves current-index]
  (let [next-recipes-num (apply + (map #(:current-val %) elves))
        next-recipes (num->digits next-recipes-num)
        all-next-recipes (-> recipes
                             (assoc (inc current-index) (first next-recipes))
                             (conj (when (second next-recipes) [(+ 2 current-index) (second next-recipes)])))]
        [all-next-recipes next-recipes]))

(defn get-next-elves
  [recipes elves]
  (mapv #(let [next-index (mod (+ 1 (:current-val %) (:current-index %)) (count recipes))]
           {:current-index next-index
            :current-val (get recipes next-index)}) elves))

(defn generate-recipes
  [recipes elves n]
  (loop [recipes recipes
         elves elves
         num-generated 0]
    (if (>= num-generated n)
      recipes
      (let [next-recipes (get-next-recipes recipes elves)
            next-elves (get-next-elves next-recipes elves)]
        (recur next-recipes next-elves (+ num-generated (- (count next-recipes) (count recipes))))))))

(defn challenge1
  [seed num-warmup-recipes]
  (let [recipes (num->digits (Integer/parseInt seed))
        elve1 {:current-index 0 :current-val (get recipes 0)}
        elve2 {:current-index 1 :current-val (get recipes 1)}
        elves [elve1 elve2]
        num-recipes-to-gen (- (+ num-warmup-recipes 10) (count recipes))
        recipes (generate-recipes recipes elves num-recipes-to-gen)]
    (apply str (take 10 (drop num-warmup-recipes recipes)))))

(defn challenge2
  [seed target-sequence]
  (let [recipes (into {} (map-indexed #(vector %1 %2) (num->digits (Integer/parseInt seed))))
        elve1 {:current-index 0 :current-val (get recipes 0)}
        elve2 {:current-index 1 :current-val (get recipes 1)}
        elves [elve1 elve2]
        target-sequence-count (count target-sequence)]
    (loop [recipes recipes
           elves elves
           current-index 1]
      (let [[all-next-recipes next-recipes] (get-next-recipes2 recipes elves current-index)
            max-possible-index (+ 2 current-index)
            last-seq (mapv #(get all-next-recipes %) (range (- (inc max-possible-index) target-sequence-count) (inc max-possible-index)))
            last-string (apply str last-seq)
            second-to-last-seq (mapv #(get all-next-recipes %) (range (- max-possible-index target-sequence-count) max-possible-index))
            second-to-last-string (apply str second-to-last-seq)]
        (cond (= last-string target-sequence) (- (count all-next-recipes) target-sequence-count)
              (= second-to-last-string target-sequence) (- (count all-next-recipes) target-sequence-count (dec (count next-recipes)))
              :else (recur all-next-recipes (get-next-elves all-next-recipes elves) (+ current-index (count next-recipes))))))))