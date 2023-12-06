(ns day-01-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   [day-01 :refer :all]))

(deftest day-1-part-1-test
  (testing "Sample Data"
    (is (= 142
           (let [lines (string/split-lines (slurp "resource/day-01-example-input-pt1.txt"))]
             (challenge-1 lines)))))
  (testing "real-data"
    (is (= 54159
           (challenge-1! false)))))

(deftest day-1-part-2-test
  (testing "Sample Data"
    (is (= 281
           (challenge-2! true))))
  (testing "real-data"
    (is (= 53866
           (challenge-2! false)))))
