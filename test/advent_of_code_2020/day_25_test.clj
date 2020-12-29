(ns advent-of-code-2020.day-25-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2020.day-25 :refer :all]))

(deftest day-25-part-1-test
  (testing "Sample Data"
    (let [input-string "5764801\n17807724"
          input (parse-input input-string)]
      (is (= 14897079
             (challenge-1 input)))))
  (testing "real-data"
    (is (= 711945
           (challenge-1!)))))

