(defpackage advent-of-code-2019/tests/day-9
  (:use :cl
        :advent-of-code-2019.day-9
        :rove))
(in-package :advent-of-code-2019/tests/day-9)


(deftest test-challenge-1
  (ok (equal 3638931938
             (advent-of-code-2019.day-9::challenge-1 "../resources/day-9-input" '(1)))))
