(defpackage advent-of-code-2019/tests/day-4
  (:use :cl
        :advent-of-code-2019.day-4
        :rove))
(in-package :advent-of-code-2019/tests/day-4)


(deftest test-is-valid-password-p
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password-p "111111" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password-p "111123" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password-p "112233" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password-p "123444" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password-p "133679" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password-p "111122" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password-p "223450" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password-p "123789" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password-p "135679" 0 999999))))

(deftest test-is-valid-password2-p
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password2-p "111111" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password2-p "111123" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password2-p "112233" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password2-p "133679" 0 999999)))
  (ok (equal T
             (advent-of-code-2019.day-4::is-valid-password2-p "111122" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password2-p "123444" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password2-p "223450" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password2-p "123789" 0 999999)))
  (ok (equal NIL
             (advent-of-code-2019.day-4::is-valid-password2-p "135679" 0 999999))))


(deftest test-num-valid-passwords
  (ok (= 460
         (advent-of-code-2019.day-4::num-valid-passwords
          #'advent-of-code-2019.day-4::is-valid-password-p
          382345
          843167)))
  (ok (= 290
         (advent-of-code-2019.day-4::num-valid-passwords
          #'advent-of-code-2019.day-4::is-valid-password2-p
          382345
          843167))))


(deftest test-challenge-1
  (ok (= 460
         (advent-of-code-2019.day-4::challenge-1 382345 843167))))


(deftest test-challenge-2
  (ok (= 290
         (advent-of-code-2019.day-4::challenge-2 382345 843167))))

