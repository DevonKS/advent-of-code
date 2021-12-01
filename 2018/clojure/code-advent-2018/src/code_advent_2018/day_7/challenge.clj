(ns code-advent-2018.day-7.challenge
  (:require [clojure.string :as s]
            [alandipert.kahn :as kahn]
            [clojure.set :refer [difference]]))

(defn parse-line
  [line]
  (let [[parent child] (rest (re-find #"Step ([A-Z]{1}) must be finished before step ([A-Z]{1}) can begin." line))]
    {:parent parent :child child}))

(defn parse-file
  [filename]
  (let [lines (s/split-lines (slurp (str "src/code_advent_2018/day_7/" filename)))
        parsed-lines (map parse-line lines)]
    (reduce (fn [result line]
              (let [{parent :parent child :child} line]
                (assoc result parent (conj (or (result parent) #{}) child))))
            {}
            parsed-lines)))

(defn challenge1
  [filename]
  (s/join (kahn/kahn-sort (parse-file filename))))

(defn get-time-remaining
  [task]
  (- (int (first task)) 4))

(defn task-in-state?
  [task state]
  (= state (:state task)))

(defn get-tasks
  [tasks state]
  (into {} (filter #(task-in-state? (second %) state) tasks)))

(defn get-default-state
  [work-graph]
  (let [tasks (keys work-graph)
        ready-tasks (kahn/no-incoming work-graph)]
    (into {} (map #(vector % {:time-remaining (get-time-remaining %)
                              :state (if (some (fn [task] (= % task)) ready-tasks) :ready :not-ready)})
                  tasks))))

(defn get-task-parents
  [task-key tasks work-graph]
  (select-keys tasks (map first (filter (fn [node] (some #(= task-key %) (second node))) work-graph))))

(defn update-not-ready-tasks
  [work-graph tasks]
  (let [newly-ready-tasks (filter (fn [task] (every? #(task-in-state? (second %) :done) (get-task-parents (first task) tasks work-graph))) (get-tasks tasks :not-ready))
        ready-tasks (map #(vector (first %) (assoc (second %) :state :ready)) newly-ready-tasks)]
    (merge tasks (into {} ready-tasks))))

(defn update-ready-tasks
  [num-workers tasks]
  (let [available-tasks (into (sorted-map) (get-tasks tasks :ready))
        num-free-workers (- num-workers (count (get-tasks tasks :in-progress)))
        tasks-to-start (take num-free-workers available-tasks)
        started-tasks (map #(vector (first %) (assoc (second %) :state :in-progress)) tasks-to-start)]
    (merge tasks (into {} started-tasks))))

(defn work-on-in-progress-tasks
  [tasks]
  (into {} (map #(if (task-in-state? (second %) :in-progress) (vector (first %) (update (second %) :time-remaining dec)) %) tasks)))

(defn update-in-progress-tasks
  [tasks]
  (let [newly-done-tasks (filter #(>= 0 (:time-remaining (second %))) tasks)
        done-tasks (map #(vector (first %) (assoc (second %) :state :done)) newly-done-tasks)]
    (merge tasks (into {} done-tasks))))

(defn challenge2
  [filename num-workers]
  (loop [work-graph (kahn/normalize (parse-file filename))
         tasks (get-default-state work-graph)
         time-taken 0]
    (if (every? #(task-in-state? (second %) :done) tasks)
      time-taken
      (let [new-tasks (->> tasks
                           (update-not-ready-tasks work-graph)
                           (update-ready-tasks num-workers)
                           (work-on-in-progress-tasks)
                           (update-in-progress-tasks))]
        (recur work-graph new-tasks (inc time-taken))))))