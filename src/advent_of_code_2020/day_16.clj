(ns advent-of-code-2020.day-16
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn parse-rule
  [rule]
  (let [[_ field-name lower-1 upper-1 lower-2 upper-2] (re-matches #"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)" rule)]
    {:field-name field-name
     :lower-1 (util/parse-int lower-1)
     :upper-1 (util/parse-int upper-1)
     :lower-2 (util/parse-int lower-2)
     :upper-2 (util/parse-int upper-2)}))

(defn parse-rules
  [rules]
  (mapv parse-rule rules))

(defn parse-ticket
  [ticket]
  (mapv util/parse-int (string/split ticket #",")))

(defn parse-tickets
  [tickets]
  (mapv parse-ticket tickets))

(defn parse-input
  [input]
  (let [[rules my-ticket nearby-tickets] (string/split input #"\n\n")]
    {:rules (parse-rules (string/split rules #"\n"))
     :my-ticket (parse-ticket (second (string/split my-ticket #"\n")))
     :nearby-tickets (parse-tickets (rest (string/split nearby-tickets #"\n")))}))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 16)))

(defn valid-value?
  [rules value]
  (some
   (fn [{:keys [lower-1 upper-1 lower-2 upper-2]}]
     (or (<= lower-1 value upper-1)
         (<= lower-2 value upper-2)))
   rules))

(defn get-invalid-values
  [rules values]
  (remove (partial valid-value? rules) values))

(defn challenge-1
  [input]
  (apply + (mapcat (partial get-invalid-values (:rules input)) (:nearby-tickets input))))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn determine-field-indices
  [rules values]
  (let [potential-field-indices
        (reduce
         (fn [result i]
           (let [matching-rules (filter
                                 (fn [rule]
                                   (let [i-values (map #(get % i) values)]
                                     (every? (partial valid-value? [rule]) i-values)))
                                 rules)]
             (if (seq matching-rules)
               (reduce
                (fn [result matching-rule]
                  (update result (:field-name matching-rule) conj i))
                result
                matching-rules)
               result)))
         {}
         (range (count rules)))

        field-indices
        (loop [potential-field-indices potential-field-indices]
          (if (every? #(= 1 (count %)) (vals potential-field-indices))
            potential-field-indices
            (let [locked-in
                  (set (mapcat #(if (= 1 (count %))
                                  %
                                  [])
                               (vals potential-field-indices)))

                  new-potential-field-indicies
                  (reduce-kv
                   (fn [m k v]
                     (let [new-v (if (= 1 (count v))
                                   v
                                   (remove locked-in v))]
                       (assoc m k new-v)))
                   {}
                   potential-field-indices)]
              (recur new-potential-field-indicies))))]
    (reduce-kv
     (fn [m k v] (assoc m k (first v)))
     {}
     field-indices)))

(defn challenge-2
  [input]
  (let [{:keys [rules nearby-tickets my-ticket]} input
        valid-nearby-tickets (remove (comp seq (partial get-invalid-values rules)) nearby-tickets)
        fields (determine-field-indices rules valid-nearby-tickets)
        interesting-fields (into []
                                 (comp
                                  (map :field-name)
                                  (filter #(string/starts-with? % "departure")))
                                 rules)
        interesting-values (map #(get my-ticket (get fields %)) interesting-fields)]
    (apply * interesting-values)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
