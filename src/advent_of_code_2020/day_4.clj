(ns advent-of-code-2020.day-4
  (:require [advent-of-code-2020.util :as util]
            [clojure.string :as string]))

(defn parse-raw-passport
  [raw-passport]
  (let [raw-fields (string/split raw-passport #"\n| ")]
    (into {}
          (map #(let [[k v] (string/split % #":")
                      k (keyword k)]
                  (case k
                    (:byr :iyr :eyr) [k (util/parse-int v)]
                    [k v])))
          raw-fields)))

(defn parse-input
  [input]
  (let [raw-passports (string/split input #"\n\n")
        passports (map parse-raw-passport raw-passports)]
    passports))

(defn parse-input!
  []
  (parse-input (util/read-challenge-file! 4)))

(defn birth-year-valid?
  [passport]
  (<= 1920 (:byr passport) 2002))

(defn issue-year-valid?
  [passport]
  (<= 2010 (:iyr passport) 2020))

(defn expiration-year-valid?
  [passport]
  (<= 2020 (:eyr passport) 2030))

(defn height-valid?
  [passport]
  (let [raw-height (:hgt passport)
        [_ height unit] (re-matches #"(\d+)(cm|in)" raw-height)
        height (util/parse-int height)]
    (case unit
      "cm" (<= 150 height 193)
      "in" (<= 59 height 76)
      false)))

(defn hair-color-valid?
  [passport]
  (re-matches #"\#[0-9a-f]{6}" (:hcl passport)))

(defn eye-color-valid?
  [passport]
  (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} (:ecl passport)))

(defn passport-id-valid?
  [passport]
  (re-matches #"[\d]{9}" (:pid passport)))

(defn all-fields-present?
  [passport]
  (let [required-keys [:hcl :ecl :iyr :pid :hgt :eyr :byr]]
    (every? (partial contains? passport) required-keys)))

;; TODO I could use spec to do this
(defn passport-valid?
  [passport]
  (let [valid? ((every-pred all-fields-present?
                            birth-year-valid?
                            issue-year-valid?
                            expiration-year-valid?
                            height-valid?
                            hair-color-valid?
                            eye-color-valid?
                            passport-id-valid?)
                passport)]
    valid?))

(defn challenge-1
  [passports]
  (util/count-if all-fields-present? passports))

(defn challenge-1!
  []
  (challenge-1 (parse-input!)))

(defn challenge-2
  [passports]
  (util/count-if passport-valid? passports))

(defn challenge-2!
  []
  (challenge-2 (parse-input!)))
