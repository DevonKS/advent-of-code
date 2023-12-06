(ns util
  (:require
   [clojure.string :as string]))

(defn challenge-filename
  [day example]
  (if example
    (format "day-%02d-example-input.txt" day)
    (format "day-%02d-input.txt" day)))

(defn parse-file
  [day example f]
  (let [filename (str "resource/" (challenge-filename day example))
        file (slurp filename)
        lines (string/split-lines file)]
    (map f lines)))

(defn read-file
  [day example]
  (slurp str "resource/" (challenge-filename day example)))
