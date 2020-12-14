(ns advent-of-code-2020.day-14
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn parse-line
  [line]
  (if (string/starts-with? line "mask")
    {:op-code :mask
     :mask (get (string/split line #" ") 2)}
    (let [[_ addr value] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
      {:op-code :mem
       :addr (util/parse-int addr)
       :value (util/parse-int value)})))

(defn parse-lines
  [lines]
  (mapv parse-line lines))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 14)))

(defn apply-mask
  [mask value]
  (let [erase-mask (Long/parseLong
                    (apply str (map #(case % \X 1 \1 0 \0 0) mask))
                    2)
        mask (Long/parseLong
              (apply str (map #(case % \X 0 \1 1 \0 0) mask))
              2)]
    (bit-or mask (bit-and erase-mask value))))

(defn perform-op
  [memory op]
  (case (:op-code op)
    :mask (assoc memory :current-mask (:mask op))
    :mem (assoc memory (:addr op) (apply-mask (:current-mask memory) (:value op)))))

(defn mem-addrs
  [mask addr]
  (let [binary-addr (string/replace (format "%36s" (Long/toString addr 2)) " " "0")
        addrs (apply
               combo/cartesian-product
               (map (fn [m-bit a-bit]
                      (case m-bit
                        \0 [a-bit]
                        \1 [1]
                        \X [1 0]))
                    mask
                    binary-addr))]
    (mapv #(Long/parseLong (apply str %) 2) addrs)))

(defn perform-op-2
  [memory op]
  (case (:op-code op)
    :mask (assoc memory :current-mask (:mask op))
    :mem (reduce
          (fn [result addr]
            (assoc result addr (:value op)))
          memory
          (mem-addrs (:current-mask memory) (:addr op)))))

(defn challenge-1
  [ops]
  (let [memory (reduce perform-op {} ops)
        sum (as-> memory m
              (dissoc m :current-mask)
              (vals m)
              (apply + m))]
    sum))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [ops]
  (let [memory (reduce perform-op-2 {} ops)
        sum (as-> memory m
              (dissoc m :current-mask)
              (vals m)
              (apply + m))]
    sum))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
