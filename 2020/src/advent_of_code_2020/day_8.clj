(ns advent-of-code-2020.day-8
  (:require [advent-of-code-2020.util :as util]))

(defn parse-line
  [line]
  (let [[_ op-code n] (re-matches #"(\w{3}) ([\+-]\d+)" line)]
    {:op-code (keyword op-code)
     :n (util/parse-int n)}))

(defn parse-lines
  [lines]
  (mapv parse-line lines))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 8)))

(defn execute-op
  [op op-num acc]
  (case (:op-code op)
    :nop {:new-op-num (inc op-num)
          :new-acc acc}

    :acc {:new-op-num (inc op-num)
          :new-acc (+ acc (:n op))}

    :jmp {:new-op-num (+ op-num (:n op))
          :new-acc acc}))

(defn run-boot-code
  [ops]
  (let [terminating-op-num (count ops)]
    (loop [current-op-num 0
           executed-ops #{}
           acc 0]
      (cond
        (= current-op-num terminating-op-num)
        {:acc acc
         :terminated? true}

        (contains? executed-ops current-op-num)
        {:acc acc
         :terminated? false}

        :else
        (let [op (nth ops current-op-num)
              {:keys [new-op-num new-acc]} (execute-op op current-op-num acc)]
          (recur new-op-num
                 (conj executed-ops current-op-num)
                 new-acc))))))

(defn all-possible-fixed-ops
  [ops]
  (let [potentially-broken-op-nums (filter
                                    (fn [idx] (#{:jmp :nop} (get-in ops [idx :op-code])))
                                    (range 0 (count ops)))]
    (for [broken-op-num potentially-broken-op-nums]
      (update-in ops [broken-op-num :op-code]
                 (fn [op-code] (case op-code :jmp :nop :nop :jmp))))))

(defn run-boot-code-autofix
  [ops]
  (let [possible-ops (cons ops (all-possible-fixed-ops ops))]
    (reduce
     (fn [result ops]
       (let [{:keys [terminated? acc]} (run-boot-code ops)]
         (if terminated?
           (reduced acc)
           result)))
     nil
     possible-ops)))

(defn challenge-1
  [ops]
  (:acc (run-boot-code ops)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [ops]
  (run-boot-code-autofix ops))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
