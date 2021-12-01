(ns advent-of-code-2020.day-21-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-21 :refer :all]))

(deftest day-21-part-1-test
  (testing "Sample Data"
    (let [input-string "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"
          lines (string/split-lines input-string)
          foods (parse-input lines)]
      (is (= 5
             (challenge-1 foods)))))
  (testing "real-data"
    (is (= 2315
           (challenge-1!)))))

(deftest day-21-part-2-test
  (testing "Sample Data"
    (let [input-string "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"
          lines (string/split-lines input-string)
          foods (parse-input lines)]
      (is (= "mxmxvkd,sqjhc,fvjkl"
             (challenge-2 foods)))))
  (testing "real-data"
    (is (= "cfzdnz,htxsjf,ttbrlvd,bbbl,lmds,cbmjz,cmbcm,dvnbh"
           (challenge-2!)))))
