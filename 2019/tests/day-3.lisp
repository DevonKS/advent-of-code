(defpackage advent-of-code-2019/tests/day-3
  (:use :cl
        :advent-of-code-2019.day-3
        :rove)
  (:import-from :fset
                :equal?
                :seq)
  (:shadowing-import-from :fset
                          :map))
(in-package :advent-of-code-2019/tests/day-3)


(deftest test-manhatten-distance
  (ok (= 6
         (advent-of-code-2019.day-3::manhatten-distance (seq 3 3) (seq 0 0))))
  (ok (= 11
         (advent-of-code-2019.day-3::manhatten-distance (seq 6 5) (seq 0 0)))))


(deftest test-parse-path-segment
  (ok (fset:equal? (map (:dir :right) (:dist 2))
                   (advent-of-code-2019.day-3::parse-path-segment "R2")))
  (ok (fset:equal? (map (:dir :left) (:dist 1))
                   (advent-of-code-2019.day-3::parse-path-segment "L1"))))


;; test: "R2,U2,L1,D1" -> [{:dir :right :dist 2} {:dir :up :dist 2} {:dir :left :dist 1} {:dir :down :dist 1}]
(deftest test-parse-path-string
  (ok (fset:equal? (list (fset:map (:dir :right) (:dist 2))
                         (fset:map (:dir :up) (:dist 2))
                         (fset:map (:dir :left) (:dist 1))
                         (fset:map (:dir :down) (:dist 1)))
                   (advent-of-code-2019.day-3::parse-path-string "R2,U2,L1,D1")))
  (ok (fset:equal? (list (fset:map (:dir :right) (:dist 22))
                         (fset:map (:dir :up) (:dist 42))
                         (fset:map (:dir :left) (:dist 51))
                         (fset:map (:dir :down) (:dist 10000000)))
                   (advent-of-code-2019.day-3::parse-path-string "R22,U42,L51,D10000000"))))


(deftest test-generate-segment-points
  (ok (fset:equal? (list (seq 3 2) (seq 4 2))
                   (advent-of-code-2019.day-3::generate-segment-points
                    (fset:map (:dir :right) (:dist 2))
                    (seq 2 2))))
  (ok (fset:equal? (list (seq 5 3) (seq 4 3) (seq 3 3) (seq 2 3))
                   (advent-of-code-2019.day-3::generate-segment-points
                    (fset:map (:dir :left) (:dist 4))
                    (seq 6 3))))
  (ok (fset:equal? (list (seq 2 -1) (seq 2 -2) (seq 2 -3))
                   (advent-of-code-2019.day-3::generate-segment-points
                    (fset:map (:dir :down) (:dist 3))
                    (seq 2 0))))
  (ok (fset:equal? (list (seq 2 3) (seq 2 4))
                   (advent-of-code-2019.day-3::generate-segment-points
                    (fset:map (:dir :up) (:dist 2))
                    (seq 2 2)))))


(deftest test-generate-wire-points
  (ok (fset:equal? (seq (seq 0 0) (seq 1 0) (seq 2 0) (seq 2 1) (seq 2 2) (seq 1 2) (seq 1 1)) 
                   (advent-of-code-2019.day-3::generate-wire-points
                    (list (fset:map (:dir :right) (:dist 2))
                          (fset:map (:dir :up) (:dist 2))
                          (fset:map (:dir :left) (:dist 1))
                          (fset:map (:dir :down) (:dist 1)))
                    (seq 0 0)))))


(deftest test-find-wire-intersections
  (ok (fset:equal? (fset:set (seq 2 1) (seq 2 2))
                   (advent-of-code-2019.day-3::find-wire-intersections
                    (seq (seq 0 0) (seq 1 0) (seq 2 0) (seq 2 1) (seq 2 2) (seq 1 2) (seq 1 1))
                    (seq (seq 2 1) (seq 10 10) (seq 11 10) (seq 12 10) (seq 2 2) (seq 11 12) (seq 11 11))))))


(deftest test-calculate-closest-intersection
    (ok (= 6
           (advent-of-code-2019.day-3::calculate-closest-intersection
            '(0 0)
            (format nil "R8,U5,L5,D3~%U7,R6,D4,L4"))))
    (ok (= 159
           (advent-of-code-2019.day-3::calculate-closest-intersection
            '(0 0)
            (format nil "R75,D30,R83,U83,L12,D49,R71,U7,L72~%U62,R66,U55,R34,D71,R55,D58,R83"))))
    (ok (equal 135
               (advent-of-code-2019.day-3::calculate-closest-intersection
                '(0 0)
                (format nil "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51~%U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))


(deftest test-calculate-least-steps-to-intersection
  (ok (= 30
         (advent-of-code-2019.day-3::calculate-least-steps-to-intersection
          '(0 0)
          (format nil "R8,U5,L5,D3~%U7,R6,D4,L4"))))
  (ok (= 610
         (advent-of-code-2019.day-3::calculate-least-steps-to-intersection
          '(0 0)
          (format nil "R75,D30,R83,U83,L12,D49,R71,U7,L72~%U62,R66,U55,R34,D71,R55,D58,R83"))))
  (ok (= 410
         (advent-of-code-2019.day-3::calculate-least-steps-to-intersection
          '(0 0)
          (format nil "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51~%U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")))))
    

(deftest test-challenge-1
  (ok (= 316
         (advent-of-code-2019.day-3::challenge-1 "../resources/day-3-input"))))
    

(deftest test-challenge-2
  (ok (= 16368
         (advent-of-code-2019.day-3::challenge-2 "../resources/day-3-input"))))


