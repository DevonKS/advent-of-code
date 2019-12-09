(defpackage advent-of-code-2019.day-8
  (:use :cl))
(in-package :advent-of-code-2019.day-8)


(defun partition (list cell-size)
  (loop for cell on list by #'(lambda (list)
                                (nthcdr cell-size list))
     collecting (subseq cell 0 cell-size)))


(defun get-image-layers (image width height)
  (partition image (* width height)))


(defun get-item-count (l item)
  (loop for i in l count (equal i item)))


(defun get-image-checksum (image width height)
  (let* ((layers (get-image-layers image width height))
         (layers-with-zero-count (map 'list (lambda (layer)
                                              (fset:map (:layer layer) (:zeros (get-item-count layer 0))))
                                      layers))
         (lowest-0-layer-with-0 (reduce (lambda (l1 l2)
                                          (if (<= (fset:@ l1 :zeros) (fset:@ l2 :zeros))
                                              l1
                                              l2))
                                        layers-with-zero-count))
         (lowest-0-layer (fset:@ lowest-0-layer-with-0 :layer)))
    (* (get-item-count lowest-0-layer 1) (get-item-count lowest-0-layer 2))))


(defun read-image (filename)
  (map 'list #'digit-char-p (string-trim '(#\Newline) (uiop:read-file-string filename))))


(defun challenge-1 (filename width height)
  (get-image-checksum (read-image filename) width height))


(defun challenge-2-list (image width height)
  (let* ((layers (get-image-layers image height width))
        (combined-layers (map 'list
                              (lambda (layer-pixels)
                                (first (remove-if (lambda (x) (equal 2 x)) layer-pixels)))
                              (apply #'map 'list #'list layers))))
    (partition combined-layers width)))


(defun challenge-2 (filename width height)
  (challenge-2-list (read-image filename) width height))


(defun print-image (image)
  (loop for row in image do (print row)))
