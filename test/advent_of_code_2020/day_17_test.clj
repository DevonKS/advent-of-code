(ns advent-of-code-2020.day-17-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-17 :refer :all]))

(deftest day-17-part-1-test
  (testing "Sample Data"
    (let [input-string ".#.
..#
###"
          cubes (parse-input input-string)]
      (is (= 112
             (challenge-1 cubes)))))
  (testing "real-data"
    (is (= 380
           (challenge-1!)))))

(deftest day-17-part-2-test
  (testing "real-data"
    (is (= 2332
           (challenge-2!)))))
