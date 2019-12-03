(defpackage advent-of-code-2019/tests/day-2
  (:use :cl
        :advent-of-code-2019.day-2
        :rove)
  (:import-from :fset
                :convert
                :seq))
(in-package :advent-of-code-2019/tests/day-2)


(deftest test-run-intcode
  (ok (fset:equal? (convert 'seq '(3500 9 10 70 2 3 11 0 99 30 40 50))
         (advent-of-code-2019.day-2::run-intcode (convert 'seq '(1 9 10 3 2 3 11 0 99 30 40 50)))))

  (ok (fset:equal? (convert 'seq '(2 0 0 0 99))
         (advent-of-code-2019.day-2::run-intcode (convert 'seq '(1 0 0 0 99)))))

  (ok (fset:equal? (convert 'seq '(2 3 0 6 99))
         (advent-of-code-2019.day-2::run-intcode (convert 'seq '(2 3 0 3 99)))))

  (ok (fset:equal? (convert 'seq '(2 4 4 5 99 9801))
         (advent-of-code-2019.day-2::run-intcode (convert 'seq '(2 4 4 5 99 0)))))

  (ok (fset:equal? (convert 'seq '(30 1 1 4 2 5 6 0 99))
         (advent-of-code-2019.day-2::run-intcode (convert 'seq '(1 1 1 4 99 5 6 0 99))))))


(deftest test-challenge-1
  (ok (= 3101878
         (advent-of-code-2019.day-2::challenge-1 "../resources/day-2-input"))))


(deftest test-challenge-2
  (ok (= 8444
         (advent-of-code-2019.day-2::challenge-2 "../resources/day-2-input"))))
