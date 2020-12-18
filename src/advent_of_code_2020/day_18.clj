(ns advent-of-code-2020.day-18
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]))

(defn parse-input!
  []
  (util/challenge-file-lines! 18))

(defn take-sub-expression
  [expression-items]
  (loop [sub-expression []
         expression-items expression-items
         parens 0]
    (let [item (first expression-items)
          sub-expression (conj sub-expression item)
          freqs (frequencies item)
          num-open (get freqs \( 0)
          num-closed (get freqs \) 0)
          parens (+ parens num-open)]
      (cond
        (or (= num-closed parens)
            (= 1 (count expression-items)))
        (-> sub-expression
            (update 0 #(string/replace-first % "(" ""))
            (update (dec (count sub-expression)) #(string/replace-first % ")" "")))

        :else
        (recur sub-expression (rest expression-items) (- parens num-closed))))))

(defn run-expression
  [expression]
  ;(println expression)
  (let [xs (string/split expression #" ")
        n (first xs)
        op (case (second xs)
             "+" +
             "*" *
             nil)
        n2 (get xs 2)
        total (cond
                (= 1 (count xs))
                (util/parse-long n)

                (string/starts-with? n "(")
                (let [sub-expression (take-sub-expression xs)
                      new-index (count sub-expression)
                      result (run-expression (string/join " " sub-expression))
                      new-expression-items (into [result]
                                                 (subvec xs new-index))
                      new-expression (string/join " " new-expression-items)]
                  (run-expression new-expression))

                (string/starts-with? n2 "(")
                (let [sub-expression (take-sub-expression (subvec xs 2))
                      new-index (+ (count sub-expression) 2)
                      result (run-expression (string/join " " sub-expression))
                      new-expression-items (into  [n (second xs) result]
                                                  (subvec xs new-index))
                      new-expression (string/join " " new-expression-items)]
                  (run-expression new-expression))

                op
                (let [result (op (util/parse-long n) (util/parse-long n2))
                      new-expression-items (into [result]
                                                 (subvec xs 3))
                      new-expression (string/join " " new-expression-items)]
                  (run-expression new-expression)))]
    total))

(defn run-expression-2
  [expression]
  (let [xs (string/split expression #" ")
        n (first xs)
        op-code (second xs)
        op (case op-code
             "+" +
             "*" *
             nil)
        n2 (get xs 2)
        total (cond
                (= 1 (count xs))
                (util/parse-long n)

                (= 3 (count xs))
                (op (util/parse-long n) (util/parse-long n2))

                (string/starts-with? n "(")
                (let [sub-expression (take-sub-expression xs)
                      new-index (count sub-expression)
                      result (run-expression-2 (string/join " " sub-expression))
                      new-expression-items (into [result]
                                                 (subvec xs new-index))
                      new-expression (string/join " " new-expression-items)]
                  (run-expression-2 new-expression))

                (string/starts-with? n2 "(")
                (let [sub-expression (take-sub-expression (subvec xs 2))
                      new-index (+ (count sub-expression) 2)
                      result (run-expression-2 (string/join " " sub-expression))
                      new-expression-items (into  [n op-code result]
                                                  (subvec xs new-index))
                      new-expression (string/join " " new-expression-items)]
                  (run-expression-2 new-expression))

                (= "+" op-code)
                (let [result (op (util/parse-long n) (util/parse-long n2))
                      new-expression-items (into [result]
                                                 (subvec xs 3))
                      new-expression (string/join " " new-expression-items)]
                  (run-expression-2 new-expression))

                :else
                (let [sub-expression (subvec xs 2)
                      result (run-expression-2 (string/join " " sub-expression))
                      new-expression-items [n op-code result]
                      new-expression (string/join " " new-expression-items)]
                  (run-expression-2 new-expression)))]
    total))

(defn challenge-1
  [expressions]
  (apply + (map run-expression expressions)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [expressions]
  (apply + (map run-expression-2 expressions)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
