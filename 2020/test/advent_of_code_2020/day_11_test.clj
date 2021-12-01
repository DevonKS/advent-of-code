(ns advent-of-code-2020.day-11-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-11 :refer :all]))

(deftest day-11-part-1-test
  (testing "Sample Data"
    (let [input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
          lines (string/split input #"\n")
          seats (parse-input lines)]
      (is (= 37
             (challenge-1 seats)))))
  (testing "real-data"
    (is (= 2238
           (challenge-1!)))))

(deftest day-11-part-2-test
  (testing "Sample Data"
    (let [input "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
          lines (string/split input #"\n")
          seats (parse-input lines)]
      (is (= 26
             (challenge-2 seats)))))
  (testing "real-data"
    (is (= 2013
           (challenge-2!)))))
