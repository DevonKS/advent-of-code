(ns day-01-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer :all]
   [day-01 :refer :all]
   util))

(deftest day-1-part-1-test
  (testing "Sample Data"
    (is (= 142
           (let [lines (string/split-lines (slurp "resource/day-01-example-input-pt1.txt"))]
             (challenge-1 lines)))))
  (testing "real-data"
    (is (= 54159
           (let [lines (util/parse-file 1 false identity)]
             (challenge-1 lines))))))

(deftest day-1-part-2-test
  (testing "Sample Data"
    (is (= 281
           (let [lines (util/parse-file 1 true identity)]
             (challenge-2 lines)))))
  (testing "real-data"
    (is (= 53866
           (let [lines (util/parse-file 1 false identity)]
             (challenge-2 lines))))))
