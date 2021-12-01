(ns advent-of-code-2020.day-19
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]))

(defn parse-rules
  [raw-rules]
  (reduce
   (fn [res raw-rule]
     (let [[_ idx sub-rules-1 sub-rules-2 c] (re-matches #"(\d+): ([\d ]*) {0,1}\|{0,1} {0,1}([\d ]*) {0,1}(\"\w\"){0,1}"
                                                         raw-rule)
           idx (util/parse-int idx)
           rule-type (if c
                       :char
                       :sub-rules)
           sub-rules (cond-> []
                       (seq sub-rules-1)
                       (conj (mapv util/parse-int (string/split sub-rules-1 #" ")))

                       (seq sub-rules-2)
                       (conj (mapv util/parse-int (string/split sub-rules-2 #" "))))
           c (some-> c
                     (string/replace "\"" "")
                     first)]
       (assoc res idx
              {:type rule-type
               :sub-rules sub-rules
               :char c})))

   {}
   raw-rules))

(defn parse-input
  [input]
  (let [[raw-rules messages] (string/split input #"\n\n")
        rules (parse-rules (string/split-lines raw-rules))
        messages (string/split-lines messages)]
    {:rules rules
     :messages messages}))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 19)))

(declare is-rule-valid? is-rules-valid?)

(def is-rules-valid?
  (memoize
   (fn [rules rule-idxs message-vec]
     (cond
       (and (empty? rule-idxs)
            (empty? message-vec))
       true

       (or (empty? rule-idxs)
           (empty? message-vec))
       false

       :else
       (reduce
        (fn [_ i]
          (let [start-message (subvec message-vec 0 (inc i))
                rest-message (subvec message-vec (inc i))]
            (if (and (is-rule-valid? rules (first rule-idxs) start-message)
                     (is-rules-valid? rules (rest rule-idxs) rest-message))
              (reduced true)
              false)))
        false
        (range 0 (count message-vec)))))))

(def is-rule-valid?
  (memoize
   (fn [rules rule-idx message]
     (let [message-vec (vec message)
           rule (get rules rule-idx)
           char-rule? (= :char (:type rule))]
       (if char-rule?
         (= message [(:char rule)])
         (some
          #(is-rules-valid? rules % message-vec)
          (:sub-rules rule)))))))

(defn count-valid
  [rules rule-idx messages]
  (util/count-if (partial is-rule-valid? rules rule-idx) messages))

(defn challenge-1
  [input]
  (count-valid (:rules input) 0 (:messages input)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [input]
  (let [new-rules (-> (:rules input)
                      (assoc 8 {:type :sub-rules
                                :sub-rules [[42] [42 8]]
                                :char nil})
                      (assoc 11 {:type :sub-rules
                                 :sub-rules [[42 31] [42 11 31]]
                                 :char nil}))]
    (count-valid new-rules 0 (:messages input))))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
