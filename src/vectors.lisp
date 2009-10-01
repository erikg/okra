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


(defun qw2rad (qw)
  "Returns QW (quaternion w-component) converted to radians."
  (* 2 (acos qw)))


(defun rad2deg (radians)
  "Returns RADIANS converted to degrees."
  (/ radians *pi/180*))


(defun rad2qw (radians)
  "Returns RADIANS converted to a quaternion w-component."
  (cos (/ radians 2)))


(defun deg2qw (degrees)
  "Returns DEGREES converted to a quaternion w-component."
  (cos (/ (deg2rad degrees) 2)))


(defun qw2deg (qw)
  "Returns QW (quaternion w-component) converted to degrees."
  (rad2deg (* 2 (acos qw))))


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


(defun vector-scale (v &rest scalars)
  "Scales every element of V by SCALARS."
  (loop for n across v
        collect (apply #'* n scalars) into m
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
  "Returns the normalised cross product of V1 and V2."
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


;;; XXX: this is most certainly bull

;; from: http://web.archive.org/web/20041029003853/http://www.j3d.org/matrix_faq/matrfaq_latest.html#Q47
(defun direction-to-quaternion (v3 &optional (radians 0))
  "Converts a direction vector V3 with an optional rotation RADIANS to a
  quaternion (format: #(w x y z))."
  (let ((sin-a (/ radians 2.0))
        (vn (vector-normalise v3)))
    (vector-normalise (vector (cos (/ radians 2.0)) (* (elt vn 0) sin-a)
                              (* (elt vn 1) sin-a) (* (elt vn 2) sin-a)))))


;;; Function Aliases

(defalias #'direction-to-quaternion 'd2q)
(defalias #'vector-add 'vadd)
(defalias #'vector-add 'v+)
(defalias #'vector-angle 'vangle)
(defalias #'vector-cross-product 'vcross)
(defalias #'vector-division 'vdiv)
(defalias #'vector-division 'v/)
(defalias #'vector-dot-product 'vdot)
(defalias #'vector-dot-product 'v.)
(defalias #'vector-from-coords 'xyz2v)
(defalias #'vector-length 'vlength)
(defalias #'vector-multiply 'vmult)
(defalias #'vector-multiply 'v*)
(defalias #'vector-normal 'vnormal)
(defalias #'vector-normalise 'vector-normalize)
(defalias #'vector-normalise 'vnormalise)
(defalias #'vector-normalise 'vnormalize)
(defalias #'vector-normalised-sum 'vector-normalized-sum)
(defalias #'vector-scale 'vscale)
(defalias #'vector-scalar-triple-product 'vstp)
(defalias #'vector-substract 'vsub)
(defalias #'vector-substract 'v-)
