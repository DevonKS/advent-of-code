(ns code-advent-2018.day-4.challenge-test
  (:require [clojure.test :refer :all]
            [code-advent-2018.day-4.challenge :refer :all]
            [java-time]))

(defn parse-date
  [date-string]
  (java-time/local-date-time "yyyy-MM-dd HH:mm" date-string))

(deftest parse-line-test
  (testing "Parse Line Test"
    (is (= {:time (parse-date "1518-04-21 00:57")  :action "wakes" :guard-id -1} (parse-line "[1518-04-21 00:57] wakes up")))
    (is (= {:time (parse-date "1518-09-03 00:12") :action "asleep" :guard-id -1} (parse-line "[1518-09-03 00:12] falls asleep")))
    (is (= {:time (parse-date "1518-04-21 00:04") :action "shift" :guard-id 3331} (parse-line "[1518-04-21 00:04] Guard #3331 begins shift")))))

(deftest challenge1-test
  (testing "Challenge 1 Test"
    (is (= 36898 (challenge1 "input.txt")))))

(deftest challenge2-test
  (testing "Challenge 2 Test"
    (is (= 80711 (challenge2 "input.txt")))))