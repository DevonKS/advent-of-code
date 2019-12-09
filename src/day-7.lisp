(defpackage advent-of-code-2019.day-7
  (:use :cl))
(in-package :advent-of-code-2019.day-7)


(defun permutations (items)
  (if (= 1 (length items))
      (list items)
      (loop for item in items
         for perms = (permutations (remove item items :test #'equal))
         for all-perms = (map 'list (lambda (perm) (cons item perm)) perms)
         append all-perms)))


(defun get-thrust (intcode phase-setting)
  (loop for phase in phase-setting
     for signal = 0 then output-signal
     for output-signal = (advent-of-code-2019.day-5::run-intcode intcode (list phase signal))
     finally (return output-signal)))


(defun find-max-thrust (intcode)
  (loop for phase-setting in (permutations '(0 1 2 3 4))
     for thrust = (get-thrust intcode phase-setting)
     maximizing thrust into max-thrust
     finally (return max-thrust)))

(defun challenge-1 (filename)
  (find-max-thrust (advent-of-code-2019.day-5::read-intcode filename)))
