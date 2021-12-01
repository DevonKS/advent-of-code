(ns advent-of-code-2020.day-13
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]))

(defn parse-lines
  [lines]
  (let [depart-timestamp (util/parse-int (first lines))
        bus-ids (into []
                      (comp
                       (map-indexed (fn [i x] [i (util/parse-int x)]))
                       (filter (fn [[_ x]] x)))
                      (string/split (second lines) #","))]
    {:depart-timestamp depart-timestamp
     :bus-ids bus-ids}))

(defn parse-input!
  []
  (parse-lines (util/challenge-file-lines! 13)))

(defn time-waiting-for-bus
  [depart-timestamp bus-id]
  (- (* (int (Math/ceil (/ depart-timestamp bus-id))) bus-id)
     depart-timestamp))

(defn challenge-1
  [input]
  (let [[wait-time bus-id] (apply
                            min-key
                            first
                            (map (fn [[_ bus-id]] (vector (time-waiting-for-bus (:depart-timestamp input) bus-id) bus-id))
                                 (:bus-ids input)))]
    (* wait-time bus-id)))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

;;;;;;;;;;;;;;;;; Chinese Remainder Theorem ;;;;;;;;;;;;;;;;;
;; I took this from https://rosettacode.org/wiki/Chinese_remainder_theorem#Clojure and slightly modified it.

(defn extended-gcd
  "The extended Euclidean algorithm
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs. "
  [a b]
  (cond (zero? a) [(Math/abs b) 0 1]
        (zero? b) [(Math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (Math/abs b)
                     r0 (Math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  " Main routine to return the chinese remainder "
  [conguences]
  (let [prod (apply * (map second conguences))
        reducer (fn [sum [a-i n-i]]
                  (let [p (quot prod n-i)
                        egcd (extended-gcd p n-i)
                        inv-p (second egcd)]
                    (+ sum (* a-i inv-p p))))
        sum-prod (reduce reducer 0 conguences)]
    (mod sum-prod prod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn challenge-2
  [input]
  (let [congruences (map (fn [[i x]] [(- x i) x]) (:bus-ids input))]
    (chinese-remainder congruences)))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
