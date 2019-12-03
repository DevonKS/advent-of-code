(defpackage advent-of-code-2019.day-1
  (:use :cl)
  (:import-from :uiop
                :read-file-lines))
(in-package :advent-of-code-2019.day-1)


(defun naive-fuel-required (mass)
  "Return the amount of fueld required to launch the mass.
   This function is naive because it assumes the weight of the fuel is neglegable for the lauch calc."
  (- (floor mass 3) 2))


(defun fuel-required (mass)
  "Returns the amount of fuel required to launch the mass.
   Takes into account the weight of the fuel."
  (loop for current-fuel-count = (naive-fuel-required mass) then (naive-fuel-required current-fuel-count)
     until (>= 0 current-fuel-count)
     summing current-fuel-count into total
     finally (return total)))


(defun total-fuel-required (module-masses fuel-required-fn)
  "Returns the sum of the fuel required for each module."
  (reduce (lambda (result module-mass)
            (+ result (funcall fuel-required-fn module-mass)))
          module-masses
          :initial-value 0))

(defun read-module-masses (filename)
  "Returns a list of module masses that have been read from filename.
   It is assumed that each line of the file contains one module-mass."
  (map 'list
       'parse-integer
       (uiop:read-file-lines filename)))

(defun challenge-1 (filename)
  "Returns the fuel required to launch of all the modules from filename."
  (total-fuel-required (read-module-masses filename) #'naive-fuel-required))


(defun challenge-2 (filename)
  "Returns the fuel required to launch of all the modules from filename." 
  (total-fuel-required (read-module-masses filename) #'fuel-required))
