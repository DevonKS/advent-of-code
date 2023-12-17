(ns day-02-test
  (:require
   [clojure.test :refer :all]
   [day-02 :refer :all]))

(deftest day-2-part-1-test
  (testing "Sample Data"
    (is (= 8
           (let [games (read-input true)]
             (challenge-1 games)))))
  (testing "real-data"
    (is (= 2447
           (let [games (read-input false)]
             (challenge-1 games))))))

(deftest day-2-part-2-test
  (testing "Sample Data"
    (is (= 2286
           (let [games (read-input true)]
             (challenge-2 games)))))
  (testing "real-data"
    (is (= 56322
           (let [games (read-input false)]
             (challenge-2 games))))))
