(defpackage advent-of-code-2019/tests/day-5
  (:use :cl
        :advent-of-code-2019.day-5
        :rove))
(in-package :advent-of-code-2019/tests/day-5)


(deftest test-challenge-1
  (ok (equal 12428642
             (advent-of-code-2019.day-5::challenge-1 "../resources/day-5-input" '(1)))))


(deftest test-challenge-2
  (ok (equal 918655
             (advent-of-code-2019.day-5::challenge-1 "../resources/day-5-input" '(5)))))


(deftest test-run-intcode
  (ok (equal 0
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 9 8 9 10 9 4 9 99 -1 8) '(7))))
  (ok (equal 1
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 9 8 9 10 9 4 9 99 -1 8) '(8))))

  (ok (equal 1
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 9 7 9 10 9 4 9 99 -1 8) '(7))))
  (ok (equal 0
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 9 7 9 10 9 4 9 99 -1 8) '(8))))
  
  (ok (equal 0
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 3 1108 -1 8 3 4 3 99) '(7))))
  (ok (equal 1
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 3 1108 -1 8 3 4 3 99) '(8))))

  (ok (equal 1
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 3 1107 -1 8 3 4 3 99) '(7))))
  (ok (equal 0
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 3 1107 -1 8 3 4 3 99) '(8))))

  (ok (equal 1
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9) '(7))))
  (ok (equal 0
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 12 6 12 15 1 13 14 13 4 13 99 -1 0 1 9) '(0))))

  (ok (equal 1
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 3 1105 -1 9 1101 0 0 12 4 12 99 1) '(7))))
  (ok (equal 0
             (advent-of-code-2019.day-5::run-intcode (fset:seq 3 3 1105 -1 9 1101 0 0 12 4 12 99 1) '(0))))

  (ok (equal 999
             (advent-of-code-2019.day-5::run-intcode
              (fset:seq 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99)
              '(7))))
  (ok (equal 1000
             (advent-of-code-2019.day-5::run-intcode
              (fset:seq 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99)
              '(8))))
  (ok (equal 1001
             (advent-of-code-2019.day-5::run-intcode
              (fset:seq 3 21 1008 21 8 20 1005 20 22 107 8 21 20 1006 20 31 1106 0 36 98 0 0 1002 21 125 20 4 20 1105 1 46 104 999 1105 1 46 1101 1000 1 20 4 20 1105 1 46 98 99)
              '(9)))))
