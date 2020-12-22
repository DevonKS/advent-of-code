(ns advent-of-code-2020.day-22-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-22 :refer :all]))

(deftest day-22-part-1-test
  (testing "Sample Data"
    (let [input-string "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"
          game-state (parse-input input-string)]
      (is (= 306
             (challenge-1 game-state)))))
  (testing "real-data"
    (is (= 30780
           (challenge-1!)))))

(deftest day-22-part-2-test
  (testing "Sample Data"
    (let [input-string "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"
          game-state (parse-input input-string)]
      (is (= 291
             (challenge-2 game-state)))))
  (testing "Sample Data"
    (let [input-string "Player 1:
43
19

Player 2:
2
29
14"
          game-state (parse-input input-string)]
      (is (= 105
             (challenge-2 game-state)))))
  (testing "real-data"
    (is (= 36621
           (challenge-2!)))))
