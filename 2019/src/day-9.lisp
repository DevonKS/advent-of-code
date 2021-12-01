(defpackage advent-of-code-2019.day-9
  (:use :cl))
(in-package :advent-of-code-2019.day-9)


(defun challenge-1 (filename inputs)
  (advent-of-code-2019.day-5::run-intcode (advent-of-code-2019.day-5::read-intcode filename) inputs))


(defun challenge-2 (filename inputs)
  (advent-of-code-2019.day-5::run-intcode (advent-of-code-2019.day-5::read-intcode filename) inputs))
