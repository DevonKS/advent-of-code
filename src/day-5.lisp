(defpackage advent-of-code-2019.day-5
  (:use :cl :cl-arrows))
(in-package :advent-of-code-2019.day-5)


(defun get-param-value (param-mode intcode index)
  (case param-mode
    (0 (fset:@ intcode (fset:@ intcode index)))
    (1 (fset:@ intcode index))))


(defun basic-op (op-request fn)
  (let* ((opcode (fset:@ op-request :opcode))
         (current-pos (fset:@ op-request :current-pos))
         (intcode (fset:@ op-request :intcode))
         (val1-param-mode (digit-char-p (aref opcode 2)))
         (val2-param-mode (digit-char-p (aref opcode 1)))
         (val1 (get-param-value val1-param-mode intcode current-pos))
         (val2 (get-param-value val2-param-mode intcode (+ 1 current-pos)))
         (dest (fset:@ intcode (+ 2 current-pos))))
    (-> op-request
        (fset:with :intcode (fset:with intcode dest (funcall fn val1 val2)))
        (fset:with :current-pos (+ 3 current-pos)))))


(defun basic-jump-op (op-request pred-fn)
  (let* ((opcode (fset:@ op-request :opcode))
         (current-pos (fset:@ op-request :current-pos))
         (intcode (fset:@ op-request :intcode))
         (val-1-param-mode (digit-char-p (aref opcode 2)))
         (val-2-param-mode (digit-char-p (aref opcode 1)))
         (val1 (get-param-value val-1-param-mode intcode current-pos))
         (val2 (get-param-value val-2-param-mode intcode (+ 1 current-pos)))
         (new-pos (if (funcall pred-fn val1)
                      val2
                      (+ 2 current-pos))))
    (fset:with op-request :current-pos new-pos)))


(defun basic-comparison-op (op-request comparison-fn)
  (let* ((opcode (fset:@ op-request :opcode))
         (intcode (fset:@ op-request :intcode))
         (current-pos (fset:@ op-request :current-pos))
         (val-1-param-mode (digit-char-p (aref opcode 2)))
         (val-2-param-mode (digit-char-p (aref opcode 1)))
         (val1 (get-param-value val-1-param-mode intcode current-pos))
         (val2 (get-param-value val-2-param-mode intcode (+ 1 current-pos)))
         (dest (fset:@ intcode (+ 2 current-pos)))
         (write-val (if (funcall comparison-fn val1 val2)
                        1
                        0)))
    (-> op-request
        (fset:with :intcode (fset:with intcode dest write-val))
        (fset:with :current-pos (+ 3 current-pos)))))


(defun read-single-digit-from-input ()
  (loop for input = (progn
                       (print "Please enter a single digit:")
                       (read-line))
     until (and (= 1 (length input))
                (digit-char-p (aref input 0)))
    finally (return (digit-char-p (aref input 0)))))


(defun op-1 (op-request)
  (basic-op op-request #'+))


(defun op-2 (op-request)
  (basic-op op-request #'*))


(defun op-3 (op-request)
  (let* ((inputs (fset:@ op-request :inputs))
         (input (if inputs
                    (first inputs)
                    (read-single-digit-from-input)))
         (intcode (fset:@ op-request :intcode))
         (current-pos (fset:@ op-request :current-pos))
         (dest (fset:@ intcode current-pos)))
    (-> op-request
        (fset:with :intcode (fset:with intcode dest input))
        (fset:with :current-pos (+ 1 current-pos))
        (fset:with :inputs (rest inputs)))))


(defun op-4 (op-request)
  (let* ((intcode (fset:@ op-request :intcode))
         (current-pos (fset:@ op-request :current-pos))
         (opcode (fset:@ op-request :opcode))
         (val-1-param-mode (digit-char-p (aref opcode 2)))
         (value (get-param-value val-1-param-mode intcode current-pos)))
    (print value)
    (-> op-request
        (fset:with :current-pos (+ 1 current-pos))
        (fset:with :output value))))


(defun op-5 (op-request)
  (basic-jump-op op-request (lambda (val) (/= 0 val))))


(defun op-6 (op-request)
  (basic-jump-op op-request (lambda (val) (= 0 val))))


(defun op-7 (op-request)
  (basic-comparison-op op-request (lambda (val1 val2) (< val1 val2))))


(defun op-8 (op-request)
  (basic-comparison-op op-request (lambda (val1 val2) (= val1 val2))))


(defun no-op (op-request)
  op-request)


(defparameter *op-funcs* (fset:map (1 #'op-1)
                                   (2 #'op-2)
                                   (3 #'op-3)
                                   (4 #'op-4)
                                   (5 #'op-5)
                                   (6 #'op-6)
                                   (7 #'op-7)
                                   (8 #'op-8)
                                   (99 #'no-op)))


(defun dispatch-op (op-request)
  (let* ((current-pos (fset:@ op-request :current-pos))
         (opcode (format nil "~5,'0D" (fset:@ (fset:@ op-request :intcode) current-pos)))
         (op (parse-integer (subseq opcode (- (length opcode) 2))))
         (op-func (fset:@ *op-funcs* op))
         (op-request* (-> op-request
                          (fset:with :opcode opcode)
                          (fset:with :op op)
                          (fset:with :current-pos (+ 1 current-pos))))
         (result (funcall op-func op-request*)))
    result))
        

(defun run-intcode (intcode inputs)
  (loop for op-result = (dispatch-op (fset:map  (:intcode  intcode) (:current-pos 0) (:inputs inputs))) then (dispatch-op op-result)
     until (= (fset:@ op-result :op) 99)
     finally (return (fset:@ op-result :output))))


(defun read-intcode (filename)
  "Returns an intcode from filename."
  (fset:convert 'fset:seq
     (map 'list
       'parse-integer
       (uiop:split-string (uiop:read-file-string filename) :separator ","))))


(defun challenge-1 (filename inputs)
  (run-intcode (read-intcode filename) inputs))


(defun challenge-2 (filename inputs)
  (run-intcode (read-intcode filename) inputs))
