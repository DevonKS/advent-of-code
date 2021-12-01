(defpackage advent-of-code-2019/tests/day-7
  (:use :cl
        :advent-of-code-2019.day-7
        :rove))
(in-package :advent-of-code-2019/tests/day-7)


(deftest test-permutations
  (ok (equal (list '(1 2 3) '(1 3 2) '(2 1 3) '(2 3 1) '(3 1 2) '(3 2 1))
             (advent-of-code-2019.day-7::permutations '(1 2 3)))))


(deftest test-find-max-thrust
  (ok (equal 43210
             (advent-of-code-2019.day-7::find-max-thrust (fset:seq 3 15 3 16 1002 16 10 16 1 16 15 15 4 15 99 0 0))))
  
  (ok (equal 54321
             (advent-of-code-2019.day-7::find-max-thrust (fset:seq 3 23 3 24 1002 24 10 24 1002 23 -1 23 101 5 23 23 1 24 23 23 4 23 99 0 0))))
  
  (ok (equal 65210
             (advent-of-code-2019.day-7::find-max-thrust (fset:seq 3 31 3 32 1002 32 10 32 1001 31 -2 31 1007 31 0 33 1002 33 7 33 1 33 31 31 1 32 31 31 4 31 99 0 0 0)))))


(deftest test-challenge-1
  (ok (equal 17440
             (advent-of-code-2019.day-7::challenge-1 "../resources/day-7-input"))))
