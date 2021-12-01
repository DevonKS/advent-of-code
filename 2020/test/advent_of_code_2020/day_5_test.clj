(ns advent-of-code-2020.day-5-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-5 :refer :all]))

(deftest parse-input-test
  (testing "Parsing"
    (let [lines (string/split "FBFBBFFRLR\nBFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL" #"\n")
          seats (parse-input lines)]
      (is (= [{:row 44, :seat 5, :seat-id 357}
              {:row 70, :seat 7, :seat-id 567}
              {:row 14, :seat 7, :seat-id 119}
              {:row 102, :seat 4, :seat-id 820}]
             seats)))))

(deftest day-5-part-1-test
  (testing "Sample Data"
    (let [lines (string/split "FBFBBFFRLR\nBFFFBBFRRR\nFFFBBBFRRR\nBBFFBBFRLL" #"\n")
          seats (parse-input lines)]
      (is (= 820
             (challenge-1 seats)))))
  (testing "real-data"
    (is (= 850
           (challenge-1!)))))

(deftest day-5-part-2-test
  (testing "real-data"
    (is (= 599
           (challenge-2!)))))
