(defpackage advent-of-code-2019.day-4
  (:use :cl))
(in-package :advent-of-code-2019.day-4)


(defun is-valid-password-p (password range-lower range-upper)
  (if (or (/= 6 (length password))
          (not (parse-integer password :junk-allowed t))
          (not (<= range-lower (parse-integer password) range-upper)))
      nil
      (loop for digit-char across password
         for previous-digit = nil then digit
         for digit = (digit-char-p digit-char)
         for meets-double-req = nil then (or meets-double-req (= previous-digit digit))  
         for meets-never-decreasing = t then (and meets-never-decreasing (<= previous-digit digit))
         finally (return (and meets-double-req meets-never-decreasing)))))


(defun is-valid-password2-p (password range-lower range-upper)
  (if (or (/= 6 (length password))
          (not (parse-integer password :junk-allowed t))
          (not (<= range-lower (parse-integer password) range-upper)))
      nil
      (loop for index = 0 then (+ 1 index)
         for digit-char across password
         for previous-digit = nil then digit
         for digit = (digit-char-p digit-char)
         for meets-double-req = nil then (if meets-double-req
                                             t
                                             (and (= previous-digit digit)
                                                  (not (equal previous-digit (ignore-errors (digit-char-p (char password (- index 2))))))
                                                  (not (equal digit (ignore-errors (digit-char-p (char password (+ index 1))))))))
         for meets-never-decreasing = t then (and meets-never-decreasing (<= previous-digit digit))
         finally (return (and meets-double-req meets-never-decreasing)))))


(defun num-valid-passwords (pred range-lower range-upper)
 (loop for num from range-lower upto range-upper
        for str-num = (write-to-string num)
        counting (funcall pred str-num range-lower range-upper) into num-valid-passwords
        finally (return num-valid-passwords)) )


(defun challenge-1 (range-lower range-upper)
  (num-valid-passwords #'is-valid-password-p range-lower range-upper))


(defun challenge-2 (range-lower range-upper)
  (num-valid-passwords #'is-valid-password2-p range-lower range-upper))
