(defpackage advent-of-code-2019/tests/day-10
  (:use :cl
        :advent-of-code-2019.day-10
        :rove))
(in-package :advent-of-code-2019/tests/day-10)


(deftest test-is-blocking
  (ok (equal T
             (advent-of-code-2019.day-10::is-blocking '(0 0) '(4 4) '(2 2))))
  (ok (equal NIL
             (advent-of-code-2019.day-10::is-blocking '(0 0) '(4 4) '(1 2))))

  (ok (equal NIL
             (advent-of-code-2019.day-10::is-blocking '(0 0) '(3 1) '(6 2))))
  (ok (equal T
             (advent-of-code-2019.day-10::is-blocking '(0 0) '(6 2) '(3 1))))
  (ok (equal T
             (advent-of-code-2019.day-10::is-blocking '(0 0) '(9 3) '(3 1)))))

