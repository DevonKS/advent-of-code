(ns main
  (:require
   [clojure.set :as set]
   [clojure.spec.alpha :as s]
   [day-01 :as d1]))

(def usage "Usage: aoc_2023_clj [OPTIONS]

Options:
  :d, :day <DAY>  [possible values: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25]
  :e, :example
  :h, :help       Print help
  :V, :version    Print version")

(def version "0.0.1")

(defn- run-aoc
  [{:keys [main/day main/example]}]
  (let [fns {1 d1/run}]
    (if-let [f (get fns day)]
      (f example)
      (doseq [[k v] fns]
        (println "Running Day:" k)
        (v example)
        (println)
        (println)))))

(defn- run
  [args]
  (cond
    (contains? args :main/help)
    (println usage)

    (contains? args :main/version)
    (println "aoc_2023_clj version" version)

    :else
    (run-aoc args)))

(defn- no-unknown-keys
  [args]
  (set/subset? (set (keys args)) #{:main/day :main/example :main/help :main/version}))

(s/def :main/day int?)
(s/def :main/example boolean?)
(s/def :main/help boolean?)
(s/def :main/version boolean?)
(s/def :main/args (s/and (s/keys :opt [:main/day :main/example :main/help :main/version])
                         no-unknown-keys))

(defn parse-args
  [args]
  (let [short->longname {:d :day :e :example :h :help :V :version}]
    (reduce
     (fn [acc [k v]]
       (let [new-k (if (contains? short->longname k) (get short->longname k) k)
             namespaced-new-k (keyword "main" (name new-k))]
         (assoc acc namespaced-new-k v)))
     {}
     args)))

(defn -main
  [args]
  (let [parsed-args (parse-args args)]
    (if (s/valid? :main/args parsed-args)
      (run parsed-args)
      (do (s/explain :main/args parsed-args)
          (println usage)))))
