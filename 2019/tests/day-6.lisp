(defpackage advent-of-code-2019/tests/day-6
  (:use :cl
        :advent-of-code-2019.day-6
        :rove))
(in-package :advent-of-code-2019/tests/day-6)

(deftest test-challenge-1
  (ok (equal 251208
             (advent-of-code-2019.day-6::challenge-1 "../resources/day-6-input")))
  (ok (equal 42
             (advent-of-code-2019.day-6::challenge-1 "../resources/day-6-test-input"))))

(deftest test-challenge-2
  (ok (equal 397
             (advent-of-code-2019.day-6::challenge-2 "../resources/day-6-input")))
  (ok (equal 4
             (advent-of-code-2019.day-6::challenge-2 "../resources/day-6-test-input2"))))
