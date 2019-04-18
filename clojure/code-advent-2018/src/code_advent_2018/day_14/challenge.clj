(ns code-advent-2018.day-14.challenge)

(defn num->digits
  [num]
  (mapv #(Integer/parseInt (str %)) (str num)))

(defn get-next-recipes
  [recipes elves]
  (let [next-recipes (num->digits (apply + (map #(get recipes %) elves)))]
    (apply conj recipes next-recipes)))

(defn get-next-recipes2
  [recipes elves current-index]
  (let [next-recipes (num->digits (apply + (map #(get recipes %) elves)))
        all-next-recipes (-> recipes
                             (assoc (inc current-index) (first next-recipes))
                             (conj (when (second next-recipes) [(+ 2 current-index) (second next-recipes)])))]
    [all-next-recipes next-recipes]))

(defn get-next-elves
  [recipes elves]
  (mapv #(mod (+ 1 (get recipes %) %) (count recipes)) elves))

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
        elves [0 1]
        num-recipes-to-gen (- (+ num-warmup-recipes 10) (count recipes))
        recipes (generate-recipes recipes elves num-recipes-to-gen)]
    (apply str (take 10 (drop num-warmup-recipes recipes)))))

(defn challenge2
  [seed target-sequence]
  (let [target-sequence-count (count target-sequence)]
    (loop [recipes (into {} (map-indexed #(vector %1 %2) (num->digits (Integer/parseInt seed))))
           elves [0 1]
           current-index 1]
      (let [[all-next-recipes next-recipes] (get-next-recipes2 recipes elves current-index)
            ranges (map #(let [end-index (+ current-index % 2)] (range (- end-index target-sequence-count) end-index)) (range 0 (count next-recipes)))
            sequences (mapv (fn [range] (apply str (map #(get all-next-recipes %) range))) ranges)
            matching-sequence-index (first (keep-indexed #(if (= %2 target-sequence) %1) sequences))]
        (if (nil? matching-sequence-index)
          (recur all-next-recipes (get-next-elves all-next-recipes elves) (+ current-index (count next-recipes)))
          (- current-index (- target-sequence-count (inc matching-sequence-index) 1)))))))