(defpackage advent-of-code-2019/tests/day-8
  (:use :cl
        :advent-of-code-2019.day-8
        :rove))
(in-package :advent-of-code-2019/tests/day-8)


(deftest test-challenge-1
  (ok (equal 1
             (advent-of-code-2019.day-8::challenge-1 "../resources/day-8-test-input" 3 2)))
  (ok (equal 1441
             (advent-of-code-2019.day-8::challenge-1 "../resources/day-8-input" 25 6))))


(deftest test-challenge-2
  (ok (equal '((1 1 1 0 0 1 0 0 1 0 1 1 1 1 0 1 1 1 0 0 1 1 1 0 0)
               (1 0 0 1 0 1 0 0 1 0 0 0 0 1 0 1 0 0 1 0 1 0 0 1 0)
               (1 0 0 1 0 1 0 0 1 0 0 0 1 0 0 1 1 1 0 0 1 0 0 1 0)
               (1 1 1 0 0 1 0 0 1 0 0 1 0 0 0 1 0 0 1 0 1 1 1 0 0)
               (1 0 1 0 0 1 0 0 1 0 1 0 0 0 0 1 0 0 1 0 1 0 0 0 0)
               (1 0 0 1 0 0 1 1 0 0 1 1 1 1 0 1 1 1 0 0 1 0 0 0 0))
             (advent-of-code-2019.day-8::challenge-2 "../resources/day-8-input" 25 6))))
