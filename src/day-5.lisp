(defpackage advent-of-code-2019.day-5
  (:use :cl))
(in-package :advent-of-code-2019.day-5)


(defun get-param-value (param-mode intcode index)
  (case param-mode
    (0 (fset:@ intcode (fset:@ intcode index)))
    (1 (fset:@ intcode index))))


(defun basic-op (opcode intcode current-pos fn)
  (let* ((val1-param-mode (digit-char-p (aref opcode 2)))
         (val2-param-mode (digit-char-p (aref opcode 1)))
         (val1 (get-param-value val1-param-mode intcode current-pos))
         (val2 (get-param-value val2-param-mode intcode (+ 1 current-pos)))
         (dest (fset:@ intcode (+ 2 current-pos))))
    (list (fset:with intcode dest (funcall fn val1 val2)) (+ 3 current-pos))))


(defun basic-jump-op (opcode intcode current-pos pred-fn)
  (let* ((val-1-param-mode (digit-char-p (aref opcode 2)))
         (val-2-param-mode (digit-char-p (aref opcode 1)))
         (val1 (get-param-value val-1-param-mode intcode current-pos))
         (val2 (get-param-value val-2-param-mode intcode (+ 1 current-pos)))
         (new-pos (if (funcall pred-fn val1)
                      val2
                      (+ 2 current-pos))))
    (list intcode new-pos)))


(defun basic-comparison-op (opcode intcode current-pos comparison-fn)
  (let* ((val-1-param-mode (digit-char-p (aref opcode 2)))
         (val-2-param-mode (digit-char-p (aref opcode 1)))
         (val1 (get-param-value val-1-param-mode intcode current-pos))
         (val2 (get-param-value val-2-param-mode intcode (+ 1 current-pos)))
         (dest (fset:@ intcode (+ 2 current-pos)))
         (write-val (if (funcall comparison-fn val1 val2)
                        1
                        0)))
    (list (fset:with intcode dest write-val) (+ 3 current-pos))))


(defun op-1 (opcode intcode current-pos)
  (basic-op opcode intcode current-pos #'+))


(defun op-2 (opcode intcode current-pos)
  (basic-op opcode intcode current-pos #'*))


(defun op-3 (opcode intcode current-pos)
  (let ((input (loop for input = (progn
                                   (print "Please enter a single digit:")
                                   (read-line))
                  until (and (= 1 (length input))
                             (digit-char-p (aref input 0)))
                  finally (return (digit-char-p (aref input 0)))))
        (dest (fset:@ intcode current-pos)))
    (list (fset:with intcode dest input) (+ 1 current-pos))))


(defun op-4 (opcode intcode current-pos)
  (let* ((val-1-param-mode (digit-char-p (aref opcode 2)))
         (value (get-param-value val-1-param-mode intcode current-pos)))
    (print value)
    (list intcode (+ 1 current-pos))))


(defun op-5 (opcode intcode current-pos)
  (basic-jump-op opcode intcode current-pos (lambda (val) (/= 0 val))))


(defun op-6 (opcode intcode current-pos)
  (basic-jump-op opcode intcode current-pos (lambda (val) (= 0 val))))


(defun op-7 (opcode intcode current-pos)
  (basic-comparison-op opcode intcode current-pos (lambda (val1 val2) (< val1 val2))))


(defun op-8 (opcode intcode current-pos)
  (basic-comparison-op opcode intcode current-pos (lambda (val1 val2) (= val1 val2))))


(defun no-op (opcode intcode current-pos)
  (list intcode current-pos))


(defparameter *op-funcs* (fset:map (1 #'op-1)
                                   (2 #'op-2)
                                   (3 #'op-3)
                                   (4 #'op-4)
                                   (5 #'op-5)
                                   (6 #'op-6)
                                   (7 #'op-7)
                                   (8 #'op-8)
                                   (99 #'no-op)))


(defun dispatch-op (intcode current-pos)
  (let* ((opcode (format nil "~5,'0D" (fset:@ intcode current-pos)))
         (op (parse-integer (subseq opcode (- (length opcode) 2))))
         (op-func (fset:@ *op-funcs* op))
         (result (funcall op-func opcode intcode (+ 1 current-pos))))
    (cons op result)))
        

(defun run-intcode (intcode)
  (loop for (op intcode* current-pos) = (dispatch-op intcode 0) then (dispatch-op intcode* current-pos)
       for final-result = nil then (if (= op 4)
                                       (fset:@ intcode* (fset:@ intcode* (- current-pos 1))) ;; FIXME this only works if the param is in position mode.
                                       final-result)
       until (= op 99)
       finally (return final-result)))


(defun read-intcode (filename)
  "Returns an intcode from filename."
  (fset:convert 'fset:seq
     (map 'list
       'parse-integer
       (uiop:split-string (uiop:read-file-string filename) :separator ","))))


(defun challenge-1 (filename)
  (run-intcode (read-intcode filename)))


(defun challenge-2 (filename)
  (run-intcode (read-intcode filename)))
