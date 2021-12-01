(defpackage advent-of-code-2019.day-2
  (:use :cl)
  (:import-from :uiop
                :read-file-lines)
  (:import-from :fset
                :with
                :convert
                :seq
                :@)
  (:import-from :cl-arrows :->))
(in-package :advent-of-code-2019.day-2)


(defun cartesian-product (s1 s2)
  "Compute the cartesian product of two lists"
  (loop for x in s1
	nconc (loop for y in s2 collect (list x y))))


(defun run-intcode (intcode)
  "Returns the results of running the intcode. `intcode` is expected to be a fset seq."
  (loop for current-index = 0 then (+ 4 current-index)
     for intcode* = intcode then (with intcode* dest new-value)
     for op = (@ intcode* current-index)
     until (= op 99)
     for val1 = (@ intcode* (@ intcode* (+ 1 current-index)))
     and val2 = (@ intcode* (@ intcode* (+ 2 current-index)))
     and dest = (@ intcode* (+ 3 current-index))
     for new-value = (case op
                       (1 (+ val1 val2))
                       (2 (* val1 val2)))
     finally (return intcode*)))


(defun run-program (intcode noun verb)
  (-> intcode
      (with 1 noun)
      (with 2 verb)
      (run-intcode)
      (@ 0)))
       

(defun read-intcode (filename)
  "Returns an intcode from filename."
  (convert 'seq
     (map 'list
       'parse-integer
       (uiop:split-string (uiop:read-file-string filename) :separator ","))))

(defun challenge-1 (filename)
  (run-program (read-intcode filename) 12 2))


(defun challenge-2 (filename)
  (let* ((intcode (read-intcode filename))
         (noun-values (loop for n from 0 below 100 collect n))
         (verb-values (loop for n from 0 below 100 collect n))
         (noun-verb-combinations (cartesian-product noun-values verb-values)))
    (loop for (noun verb) in noun-verb-combinations
         for output = (run-program intcode noun verb)
         until (= 19690720 output)
       finally (progn
                 (print output)
                 (return (+ (* 100 noun) verb))))))

