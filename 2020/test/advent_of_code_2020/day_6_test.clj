(ns advent-of-code-2020.day-6-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-6 :refer :all]))

(deftest day-6-part-1-test
  (testing "Sample Data"
    (let [groups (parse-input "abc

a
b
c

ab
ac

a
a
a
a

b")]
      (is (= 11
             (challenge-1 groups)))))
  (testing "real-data"
    (is (= 7283
           (challenge-1!)))))

(deftest day-6-part-2-test
  (testing "Sample Data"
    (let [groups (parse-input "abc

a
b
c

ab
ac

a
a
a
a

b")]
      (is (= 6
             (challenge-2 groups)))))
  (testing "real-data"
    (is (= 3520
           (challenge-2!)))))
