;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; vectors.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

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
  (loop with v1 = (copy-seq (first vectors))
        for v in (rest vectors)
        do (loop for i from 0 below (length v1)
                 do (incf (elt v1 i) (elt v i)))
        finally (return v1)))


(defun vector-cross-product (v1 v2)
  "Returns the cross product of vectors V1 and V2.  Since the vector cross
  product is only defined in three (or seven) dimensions this function
  assumes that both V1 and V2 are of length 3."
  (let ((x1 (svref v1 0)) (x2 (svref v2 0))
        (y1 (svref v1 1)) (y2 (svref v2 1))
        (z1 (svref v1 2)) (z2 (svref v2 2)))
    (vector (- (* y1 z2) (* z1 y2))
            (- (* z1 x2) (* x1 z2))
            (- (* x1 y2) (* y1 x2)))))


(defun vector-division (&rest vectors)
  "Returns the scalar division of VECTORS."
  (loop with v1 = (copy-seq (first vectors))
        for v in (rest vectors)
        do (loop for i from 0 below (length v1)
                 do (setf (elt v1 i) (/ (elt v1 i) (elt v i))))
        finally (return v1)))


(defun vector-dot-product (&rest vectors)
  "Returns the dot product of VECTORS."
  (loop with v1 = (copy-seq (first vectors))
        for v in (rest vectors)
        do (loop for i from 0 below (length v1)
                 do (setf (elt v1 i) (* (elt v1 i) (elt v i))))
        finally (return (reduce #'+ v1))))


(defun vector-from-coords (x1 y1 z1 x2 y2 z2)
  "Returns the vector between point (X1,Y1,Z1) to (X2,Y2,Z2)."
  (vector (- x2 x1) (- y2 y1) (- z2 z1)))


(defun xyz2v (x1 y1 z1 x2 y2 z2)
  "Calls VECTOR-FROM-COORDS."
  (vector-from-coords x1 y1 z1 x2 y2 z2))


(defun vector-length (v)
  "Returns the length of vector V."
  (loop for n across v
        sum (* n n) into m
        finally (return (sqrt m))))


(defun vector-multiply (&rest vectors)
  "Returns the scalar product of VECTORS."
  (loop with v1 = (copy-seq (first vectors))
        for v in (rest vectors)
        do (loop for i from 0 below (length v1)
                 do (setf (elt v1 i) (* (elt v1 i) (elt v i))))
        finally (return v1)))


(defun vector-normalise (v)
  "Returns the normalised vector V."
  (let ((length (vector-length v)))
    (loop for n across v
          collect (/ n length) into m
          finally (return (coerce m 'vector)))))


(defun vector-normalize (v)
  "Calls VECTOR-NORMALISE."
  (vector-normalise v))


(defun vector-scale (v scaling-factor)
  "Scales every element of V by SCALING-FACTOR."
  (loop for n across v
        collect (* n scaling-factor) into m
        finally (return (coerce m 'vector))))


(defun vector-scalar-triple-product (v1 v2 v3)
  "Returns the scalar triple product of V1, V2 and V3."
  (vector-dot-product (vector-cross-product v1 v2) v3))


(defun vector-substract (&rest vectors)
  "Substracts VECTORS and returns the result."
  (loop with v1 = (copy-seq (first vectors))
        for v in (rest vectors)
        do (loop for i from 0 below (length v1)
                 do (decf (elt v1 i) (elt v i)))
        finally (return v1)))


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
