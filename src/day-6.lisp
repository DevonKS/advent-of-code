(defpackage advent-of-code-2019.day-6
  (:use :cl))
(in-package :advent-of-code-2019.day-6)


(defun create-tree (relationships)
  )


(defun get-values-tree (tree)
  )


(defun get-parent-tree (tree value)
  (if (any #(contains? % value) (rest tree))
      (first tree)
      (loop for child in tree
            for parent = (get-parent child value)
            until parent
         finally (return parent))))


(defun get-depth-tree (tree value)
   (loop for parent = (get-parent tree value) then (get-parent tree parent)
         for depth = 0 then (+ 1 depth)
         until parent = nil
      finally (return depth)))


(defun sum-depths-tree (tree values)
  (loop for value in values
     for depth = (get-depth tree value)
     summing depth into total-depth
     finally (return total-depth)))


(defun read-relationships (filename)
  (reduce
   (lambda (relationships relationship)
     (let* ((parent-child (uiop:split-string relationship :separator ")"))
            (parent (first parent-child))
            (child (second parent-child)))
        (fset:with relationships child parent)))
   (uiop:read-file-lines filename)
   :initial-value (fset:empty-map)))


(defun get-ancestors (relationships value)
  (loop for parent = (fset:@ relationships value) then (fset:@ relationships parent)
     for depth = 0 then (+ 1 depth)
     until (not parent)
     collect parent))


(defun get-depth (relationships value)
  (loop for parent = (fset:@ relationships value) then (fset:@ relationships parent)
     for depth = 0 then (+ 1 depth)
     until (not parent)
     finally (return depth)))


(defun get-values (relationships)
  (fset:reduce
   (lambda (result k v)
     (cons k result))
   relationships
   :initial-value '()))


(defun sum-depths (relationships)
  (loop for key in (get-values relationships)
     for depth = (get-depth relationships key)
     summing depth into total-depth
     finally (return total-depth)))


(defun challenge-1 (filename)
  (sum-depths (read-relationships filename)))


(defun challenge-2 (filename)
  (let* ((relationships (read-relationships filename))
         (my-ancestors (get-ancestors relationships "YOU"))
         (santa-ancestors (get-ancestors relationships "SAN"))
         (common-ancestor (first (remove-if-not (lambda (item)
                                                  (member item santa-ancestors :test #' equal))
                                                my-ancestors)))
         (num-orbital-transfers (+ (position common-ancestor my-ancestors :test #'equal)
                                   (position common-ancestor santa-ancestors :test #'equal))))
    num-orbital-transfers))

