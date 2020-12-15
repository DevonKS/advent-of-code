(ns advent-of-code-2020.day-15-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [advent-of-code-2020.day-15 :refer :all]))

(deftest day-15-part-1-test
  (testing "Sample Data 1"
    (is (= 436
           (challenge-1 [0 3 6]))))
  (testing "Sample Data 2"
    (is (= 1
           (challenge-1 [1 3 2]))))
  (testing "Sample Data 3"
    (is (= 10
           (challenge-1 [2 1 3]))))
  (testing "Sample Data 4"
    (is (= 27
           (challenge-1 [1 2 3]))))
  (testing "Sample Data 5"
    (is (= 78
           (challenge-1 [2 3 1]))))
  (testing "Sample Data 6"
    (is (= 438
           (challenge-1 [3 2 1]))))
  (testing "Sample Data 7"
    (is (= 1836
           (challenge-1 [3 1 2]))))
  (testing "real-data"
    (is (= 870
           (challenge-1!)))))

(deftest day-15-part-2-test
  ;; Commenting these out since they take too long. 
  ;; Maybe if I optimise the code I can reenable them.
  #_#_#_#_#_#_#_(testing "Sample Data 1"
                  (is (= 175594
                         (challenge-2 [0 3 6]))))
              (testing "Sample Data 2"
                (is (= 2578
                       (challenge-2 [1 3 2]))))
            (testing "Sample Data 3"
              (is (= 3544142
                     (challenge-2 [2 1 3]))))
          (testing "Sample Data 4"
            (is (= 261214
                   (challenge-2 [1 2 3]))))
        (testing "Sample Data 5"
          (is (= 6895259
                 (challenge-2 [2 3 1]))))
      (testing "Sample Data 6"
        (is (= 18
               (challenge-2 [3 2 1]))))
    (testing "Sample Data 7"
      (is (= 362
             (challenge-2 [3 1 2]))))
  (testing "real-data"
    (is (= 9136
           (challenge-2!)))))
