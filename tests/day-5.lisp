(defpackage advent-of-code-2019/tests/day-5
  (:use :cl
        :advent-of-code-2019.day-5
        :rove))
(in-package :advent-of-code-2019/tests/day-5)


(deftest test-challenge-1 ;; FIXME not the best to test cause it wants input
  (ok (equal 12428642
             (advent-of-code-2019.day-5::challenge-1 "../resources/day-5-input"))))
