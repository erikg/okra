;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; vectors.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; XXX: Why didn't I use #'map in some of the functions here?!

(in-package :okra)


;;; Functions

(defun deg2rad (degrees)
  "Returns DEGREES converted to radians."
  (* degrees *pi/180*))


(defun rad2deg (radians)
  "Returns RADIANS converted to degrees."
  (/ radians *pi/180*))


(defun vector-add (&rest vectors)
  "Adds VECTORS and returns the result."
  (let ((x 0) (y 0) (z 0))
    (dolist (v vectors)
      (incf x (svref v 0))
      (incf y (svref v 1))
      (incf z (svref v 2)))
    (vector x y z)))


(defun vector-cross-product (v1 v2)
  "Returns the cross product of vectors V1 and V2."
  (let ((x1 (svref v1 0)) (x2 (svref v2 0))
        (y1 (svref v1 1)) (y2 (svref v2 1))
        (z1 (svref v1 2)) (z2 (svref v2 2)))
    (vector (- (* y1 z2) (* z1 y2))
            (- (* z1 x2) (* x1 z2))
            (- (* x1 y2) (* y1 x2)))))


(defun vector-dot-product (v1 v2)
  "Returns the dot product of vectors V1 and V2."
  (+ (* (svref v1 0) (svref v2 0))
     (* (svref v1 1) (svref v2 1))
     (* (svref v1 2) (svref v2 2))))


(defun vector-from-coords (x1 y1 z1 x2 y2 z2)
  "Returns the vector between point (X1,Y1,Z1) to (X2,Y2,Z2)."
  (vector (- x2 x1) (- y2 y1) (- z2 z1)))


(defun vector-length (v)
  "Returns the length of vector V."
  (let ((x (svref v 0))
        (y (svref v 1))
        (z (svref v 2)))
    (sqrt (+ (* x x) (* y y) (* z z)))))


(defun vector-normalise (v)
  "Returns the normalised vector V."
  (let ((length (vector-length v)))
    (if (equalp length 0.0)
        v
        (vector (/ (svref v 0) length)
                (/ (svref v 1) length)
                (/ (svref v 2) length)))))


(defun vector-normalize (v)
  "Calls VECTOR-NORMALISE."
  (vector-normalise v))


(defun vector-substract (&rest vectors)
  "Substracts VECTORS and returns the result."
  (when vectors
    (let* ((v (car vectors))
           (x (svref v 0))
           (y (svref v 1))
           (z (svref v 2)))
      (dolist (v (cdr vectors))
        (decf x (svref v 0))
        (decf y (svref v 1))
        (decf z (svref v 2)))
      (vector x y z))))


(defun vector-angle (v1 v2)
  "Returns the smallest angle from vector V1 to V2."
  (acos (vector-dot-product (vector-normalise v1) (vector-normalise v2))))


(defun vector-normal (v1 v2)
  (vector-normalise (vector-cross-product v1 v2)))


;; see: http://www.lighthouse3d.com/opengl/terrain/index.php3?normals
;; XXX: this might not work correctly yet
(defun vector-normalised-sum (&rest vectors)
  "VECTORS should be a list of vectors originating from a vertex in
  anti-clockwise manner. Assume VECTORS is '(v1 v2 v3 v4): this function then
  returns the normalised sum of the vertex normals: v12, v23, v34 and v41."
  (if (> (length vectors) 2)
      (let ((current-vector (first vectors))
            (first-vector (first vectors))
            (vector-normals nil))
        (dolist (next-vector (cdr vectors))
          (push (vector-normal current-vector next-vector) vector-normals)
          (setf current-vector next-vector))
        (push (vector-normal current-vector first-vector) vector-normals)
        (vector-normalise (apply 'vector-add vector-normals)))
    (error (mkstr "This function needs at least three vectors, otherwise use "
                  "VECTOR-NORMAL."))))


(defun vector-normalized-sum (&rest vectors)
  "Calls VECTORS-NORMALISED-SUM."
  (vector-normalised-sum vectors))
