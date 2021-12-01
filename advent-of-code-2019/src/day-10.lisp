(defpackage advent-of-code-2019.day-10
  (:use :cl))
(in-package :advent-of-code-2019.day-10)


(defun distance (p1 p2)
  (sqrt (+ (expt (- (first p2) (first p1)) 2)
           (expt (- (second p2) (second p1)) 2))))


(defun is-blocking (origin p1 p2)
  (= (distance origin p1)
     (+ (distance origin p2)
        (distance p2 p1))))


(defun visible-points (point points)
  (remove-if (lambda (p1)
               (let ((points-without-p1 (remove p1 points :test #'equal)))
                 (some (lambda (p2)
                         (is-blocking point p1 p2))
                       points-without-1)))
             points))


(defun num-visible-points (point points)
  (length (visible-points point points)))


(defun get-points (filename)
  (let* ((line (uiop:read-file-lines filename)))))


(defun challenge-1 (filename)
  (let ((points (get-points filename)))
    (apply #'max (map 'list
                    (lambda (p)
                      (num-visible-points p (remove p points :test #'equal)))
                    points))))

