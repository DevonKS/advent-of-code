(ns advent-of-code-2020.day-2-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-2 :refer :all]))

(deftest day-2-part-1-test
  (testing "Sample Data"
    (let [lines (string/split "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" #"\n")
          passwords (map parse-line lines)]
      (is (= 2
             (challenge-1 passwords)))))
  (testing "real-data"
    (is (= 666
           (challenge-1!)))))

(deftest day-2-part-2-test
  (testing "Sample Data"
    (let [lines (string/split "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" #"\n")
          passwords (map parse-line lines)]
      (is (= 1
             (challenge-2 passwords)))))
  (testing "real-data"
    (is (= 670
           (challenge-2!)))))
