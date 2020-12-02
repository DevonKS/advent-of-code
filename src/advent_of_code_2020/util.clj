(ns advent-of-code-2020.util
  (:require [clojure.java.io :as io]))

(defn challenge-filename
  [day part]
  (format "day%02d-part%02d.txt" day part))

(defn read-challenge-file!
  [day part]
  (some-> (challenge-filename day part)
          io/resource
          slurp))

(defn challenge-file-lines!
  "Returns a lazy sequence of the challenge file lines."
  [day part]
  (some-> (challenge-filename day part)
          io/resource
          io/reader
          line-seq))

(defn mapping
  [key-fn val-fn coll]
  (persistent!
   (reduce
    (fn [m x] (assoc! m (key-fn x) (val-fn x)))
    (transient {})
    coll)))

(defn map-to
  [val-fn coll]
  (mapping identity val-fn coll))

(defn map-by
  [key-fn coll]
  (mapping key-fn identity coll))
