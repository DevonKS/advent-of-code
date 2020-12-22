(ns advent-of-code-2020.day-21
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as cset]))

(defn parse-food
  [raw-food]
  (let [[_ ingredients allergens] (re-matches #"([\w ]+) \(contains ([\w, ]+)\)" raw-food)]
    {:allergens (set (string/split allergens #", "))
     :ingredients (set (string/split ingredients #" "))}))

(defn parse-input
  [lines]
  (mapv parse-food lines))

(defn parse-input!
  []
  (parse-input (util/challenge-file-lines! 21)))

(defn determine-ingredient-allergens
  [foods]
  (let [allergens (into [] (mapcat :allergens) foods)
        num-allergens (count allergens)
        allergen-potential-ingredients (reduce
                                        (fn [result food]
                                          (let [ingredients (:ingredients food)]
                                            (reduce
                                             (fn [result allergen]
                                               (update result allergen conj ingredients))
                                             result
                                             (:allergens food))))
                                        {}
                                        foods)]
    (loop [known-allergens {}
           allergen-potential-ingredients allergen-potential-ingredients]
      (let [{:keys [new-known-allergens new-allergen-potential-ingredients]}
            (reduce
             (fn [result [allergen potential-ingredients]]
               (let [ingredient (cond
                                  (> (count potential-ingredients) 1)
                                  (let [ingredients (apply cset/intersection potential-ingredients)]
                                    (if (= 1 (count ingredients))
                                      (first ingredients)
                                      nil))

                                  (and (= 1 (count potential-ingredients))
                                       (= 1 (count (first potential-ingredients))))
                                  (first (first potential-ingredients))

                                  :else
                                  nil)]
                 (if ingredient
                   (-> result
                       (update-in [:new-known-allergens allergen] (constantly ingredient))
                       (update :new-allergen-potential-ingredients (fn [x] (dissoc x allergen)))
                       (update :new-allergen-potential-ingredients
                               (fn [x]
                                 (util/mapping
                                  first
                                  (fn [[_ v]] (mapv #(disj % ingredient) v))
                                  x))))
                   result)))
             {:new-known-allergens known-allergens
              :new-allergen-potential-ingredients allergen-potential-ingredients}
             allergen-potential-ingredients)]
        (if (or (= num-allergens (count new-known-allergens))
                (= allergen-potential-ingredients
                   new-allergen-potential-ingredients))
          new-known-allergens
          (recur new-known-allergens new-allergen-potential-ingredients))))))

(defn challenge-1
  [foods]
  (let [ingredient-allergens (determine-ingredient-allergens foods)
        no-listed-allergens (into []
                                  (comp
                                   (mapcat :ingredients)
                                   (remove (set (vals ingredient-allergens))))
                                  foods)]
    (count no-listed-allergens)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [foods]
  (let [ingredient-allergens (determine-ingredient-allergens foods)
        canonical-dangerous-ingredient-list (->> ingredient-allergens
                                                 (sort-by first)
                                                 (map second)
                                                 (string/join ","))]
    canonical-dangerous-ingredient-list))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
