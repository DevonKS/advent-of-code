(ns code-advent-2018.day-8.challenge
  (:require [clojure.string :as s]))

(defn parse-file
  [filename]
  (map #(Integer/parseInt %) (s/split (s/trim (slurp (str "src/code_advent_2018/day_8/" filename))) #" ")))

(defn parse-tree
  ([raw-tree] (first (first (parse-tree raw-tree 1))))
  ([raw-tree num-nodes]
   (loop [raw-tree raw-tree
          nodes-remaining num-nodes
          nodes []]
     (if (zero? nodes-remaining)
       [nodes raw-tree]
       (let [[num-children num-metadata] (take 2 raw-tree)
             rest-raw-tree (drop 2 raw-tree)
             [children rest-raw-tree] (if (not (zero? num-children))
                                        (parse-tree rest-raw-tree num-children)
                                        [nil rest-raw-tree])
             metadata (take num-metadata rest-raw-tree)]
         (recur (drop num-metadata rest-raw-tree)
                (dec nodes-remaining)
                (conj nodes (if (nil? children)
                              {:metadata metadata}
                              {:metadata metadata :children children}))))))))

(defn sum-tree-metadata
  [tree]
  (apply + (concat (:metadata tree)
                   (if-let [children (:children tree)]
                     (map sum-tree-metadata (:children tree))
                     []))))

(defn challenge1
  [filename]
  (let [raw-tree (parse-file filename)
        tree (parse-tree raw-tree)]
    (sum-tree-metadata tree)))

(defn find-tree-value
  [tree]
  (if-let [children (:children tree)]
    (apply + (map #(if-let [child (get children (- % 1))] (find-tree-value child) 0) (:metadata tree)))
    (apply + (:metadata tree))))

(defn challenge2
  [filename]
  (let [tree (parse-tree (parse-file filename))]
    (find-tree-value tree)))