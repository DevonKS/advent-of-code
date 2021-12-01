(defpackage advent-of-code-2019.day-3
  (:use :cl)
  (:import-from :uiop
                :read-file-lines)
  (:import-from :fset
                :convert
                :seq
                :@)
  (:shadowing-import-from :fset
                          :intersection)
  (:import-from :cl-arrows :->))
(in-package :advent-of-code-2019.day-3)


(defun manhatten-distance (p1 p2)
  (+ (abs (- (@ p1 0) (@ p2 0)))
     (abs (- (@ p1 1) (@ p2 1)))))

(defun parse-path-segment (path-segment)
  (let ((dir (case (char path-segment 0)
               (#\R :right)
               (#\L :left)
               (#\U :up)
               (#\D :down)))
        (dist (parse-integer (subseq path-segment 1))))
    (fset:map (:dir dir) (:dist dist))))


(defun parse-path-string (path-string)
  (map 'list
       #'parse-path-segment
       (uiop:split-string path-string :separator ",")))


(defun read-wire-paths-from-strings (wire-path-strings)
  (map 'list #'parse-path-string wire-path-strings))


(defun generate-segment-points (segment starting-point)
  (let* ((starting-x (@ starting-point 0))
         (starting-y (@ starting-point 1))
         (dir (@ segment :dir))
         (dist (@ segment :dist))
         (x-values (case dir
                    (:right (loop for i from (+ 1 starting-x) upto (+ 1 starting-x dist) collect i))
                    (:left (loop for i from (- starting-x 1) downto (- starting-x dist 1) collect i))
                    (:up (make-list dist :initial-element starting-x))
                    (:down (make-list dist :initial-element starting-x))))
         (y-values (case dir
                    (:right (make-list dist :initial-element starting-y))
                    (:left (make-list dist :initial-element starting-y))
                    (:up (loop for i from (+ 1 starting-y) upto (+ 1 starting-y dist) collect i))
                    (:down (loop for i from (- starting-y 1) downto (- starting-y dist 1) collect i)))))
    (loop for x in x-values
                        for y in y-values
                        collect (seq x y))))


(defun generate-wire-points (path starting-point)
  (loop for starting-point* = starting-point then (fset:last segment-points)
       for path-segment in path
       for segment-points = (generate-segment-points path-segment starting-point*)
       append segment-points into result
     finally (return (convert 'seq (cons starting-point result)))))


(defun find-wire-intersections (wire-1-points wire-2-points)
  (let ((s1 (convert 'fset:set wire-1-points))
        (s2 (convert 'fset:set wire-2-points)))
    (intersection s1 s2)))


(defun calculate-closest-intersection (origin input-str)
  (let* ((wire-paths (read-wire-paths-from-strings (uiop:split-string input-str :separator '(#\newline))))
         (wire-1-path (first wire-paths))
         (wire-2-path (second wire-paths))
         (wire-1-points (generate-wire-points wire-1-path origin))
         (wire-2-points (generate-wire-points wire-2-path origin))
         (intersections (fset:less (find-wire-intersections wire-1-points wire-2-points) origin))
         (distances-from-origin (fset:image (lambda (p) (manhatten-distance p origin))
                                            (convert 'seq intersections))))
    (apply #'min (convert 'list distances-from-origin))))


(defun calculate-steps-to-intersection (intersection wire-1-points wire-2-points)
  (+ (fset:position intersection wire-1-points)
     (fset:position intersection wire-2-points)))


(defun calculate-least-steps-to-intersection (origin input-str)
  (let* ((wire-paths (read-wire-paths-from-strings (uiop:split-string input-str :separator '(#\newline))))
         (wire-1-path (first wire-paths))
         (wire-2-path (second wire-paths))
         (wire-1-points (generate-wire-points wire-1-path origin))
         (wire-2-points (generate-wire-points wire-2-path origin))
         (intersections (fset:less (find-wire-intersections wire-1-points wire-2-points) origin))
         (steps-to-intersections (fset:image
                                  (lambda (p) (calculate-steps-to-intersection p wire-1-points wire-2-points))
                                  (convert 'seq intersections))))
     (apply #'min (convert 'list steps-to-intersections))))


(defun challenge-1 (filename)
  (calculate-closest-intersection (seq 0 0) (uiop:read-file-string filename)))


(defun challenge-2 (filename)
  (calculate-least-steps-to-intersection (seq 0 0) (uiop:read-file-string filename)))
