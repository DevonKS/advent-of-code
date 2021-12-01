(ns advent-of-code-2020.day-7
  (:require [advent-of-code-2020.util :as util]
            [clojure.set :as set]
            [clojure.string :as string]))

(defn parse-child
  [child]
  (let [[_ quantity color] (re-matches #"(\d+) ([\w ]+) bags?" child)]
    {:quantity (util/parse-int quantity)
     :color color}))

(defn parse-rule
  [rule]
  (let [[_ parent-color raw-children] (re-matches #"([\w ]+) bags contain ([\w\d ,]+)\." rule)
        children (if (or (not raw-children)
                         (string/starts-with? raw-children "no"))
                   nil
                   (map parse-child (string/split raw-children #", ")))]
    {:parent parent-color
     :children children}))

(defn parse-input
  [input]
  (map parse-rule (string/split input #"\n")))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 7)))

(defn get-ancestors
  [graph node-name]
  (let [ps (into #{}
                 (comp (filter (fn [node]
                                 (some #(= node-name (:color %)) (:children node))))
                       (map :parent))
                 graph)]
    (if (seq ps)
      (reduce
       (fn [result p]
         (set/union result (get-ancestors graph p)))
       ps
       ps)
      ps)))

(defn count-children
  [graph node-name]
  (let [children (->> graph
                      (filter #(= node-name (:parent %)))
                      (mapcat :children))]
    (if (seq children)
      (apply + (map
                (fn [{:keys [quantity color]}]
                  (let [num-children (count-children graph color)]
                    (if (zero? num-children)
                      quantity
                      (+ quantity (* quantity (count-children graph color))))))
                children))
      0)))

(defn challenge-1
  [graph]
  (count (get-ancestors graph "shiny gold")))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [graph]
  (count-children graph "shiny gold"))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
