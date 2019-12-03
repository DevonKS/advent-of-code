(defpackage advent-of-code-2019/tests/day-1
  (:use :cl
        :advent-of-code-2019.day-1
        :rove))
(in-package :advent-of-code-2019/tests/day-1)

;; NOTE: To run this test file, execute `(asdf:test-system :advent-of-code-2019)' in your Lisp.

(deftest test-naive-fuel-required
    (ok (= 2
           (advent-of-code-2019.day-1::naive-fuel-required 12)))
    (ok (= 2
           (advent-of-code-2019.day-1::naive-fuel-required 14)))
    (ok (= 654
           (advent-of-code-2019.day-1::naive-fuel-required 1969)))
    (ok (= 33583
           (advent-of-code-2019.day-1::naive-fuel-required 100756))))


(deftest test-fuel-required
    (ok (= 2
           (advent-of-code-2019.day-1::fuel-required 12)))
    (ok (= 2
           (advent-of-code-2019.day-1::fuel-required 14)))
    (ok (= 966
           (advent-of-code-2019.day-1::fuel-required 1969)))
    (ok (= 50346
           (advent-of-code-2019.day-1::fuel-required 100756))))


(deftest test-total-fuel-required
    (ok (= 656
           (advent-of-code-2019.day-1::total-fuel-required '(12 1969) #'advent-of-code-2019.day-1::naive-fuel-required)))
    (ok (= 968
           (advent-of-code-2019.day-1::total-fuel-required '(12 1969) #'advent-of-code-2019.day-1::fuel-required))))


(deftest test-challenge-1
    (ok (= 3373568
           (advent-of-code-2019.day-1::challenge-1 "../resources/day-1-input"))))


(deftest test-challenge-2
    (ok (= 5057481
           (advent-of-code-2019.day-1::challenge-2 "../resources/day-1-input"))))
