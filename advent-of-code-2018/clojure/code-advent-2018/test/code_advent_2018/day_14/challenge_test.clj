(ns code-advent-2018.day-14.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-14.challenge :refer :all]))

(deftest challenge1-test
  (testing "Challenge1 Test"
    (is (= "5158916779" (challenge1 "37" 9)))
    (is (= "0124515891" (challenge1 "37" 5)))
    (is (= "9251071085" (challenge1 "37" 18)))
    (is (= "5941429882" (challenge1 "37" 2018)))
    (is (= "6521571010" (challenge1 "37" 360781)))))

(deftest challenge2-test
    (testing "Challenge2 Test"
        (is (= 9 (challenge2 "37" "51589")))
        (is (= 5 (challenge2 "37" "01245")))
        (is (= 18 (challenge2 "37" "92510")))
        (is (= 2018 (challenge2 "37" "59414")))
        (is (= 20262967 (challenge2 "37" "360781")))))