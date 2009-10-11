;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; simplex-noise.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; From: http://staffwww.itn.liu.se/~stegu/simplexnoise/simplexnoise.pdf

(in-package :okra)


;;;# Variables

(defparameter +pn-perm+ +perlin-noise-permutation+)

(defparameter +f2+ 0.366025403)  ; f2 = 0.5 * (sqrt(3.0) - 1.0)
(defparameter +g2+ 0.211324865)  ; g2 = (3.0 - sqrt(3.0)) / 6.0

(defparameter +f3+ 0.333333333)
(defparameter +g3+ 0.166666667)


;;;# Functions

(defun sngrad1d (hash x)
  (let* ((h (logand hash 15))
         (grad (+ 1.0 (logand h 7))))
    (if (= (logand h 8) 0)
        (- (* grad x))
        (* grad x))))


(defun sngrad2d (hash x y)
  (let* ((h (logand hash 7))
         (u (if (< h 4) x y))
         (v (if (< h 4) y x)))
    (+ (if (= (logand h 1) 0) (- u) u)
       (if (= (logand h 2) 0) (* -2.0 v) (* 2.0 v)))))


(defun sngrad3d (hash x y z)
  (let* ((h (logand hash 15))
         (u (if (< h 8) x y))
         (v (if (< h 4) y (if (or (= h 12) (= h 14)) x z))))
    (+ (if (= (logand h 1) 0) (- u) u)
       (if (= (logand h 2) 0) (- v) v))))


(defun sngrad4d (hash x y z tt)
  (let* ((h (logand hash 31))
         (u (if (< h 24) x y))
         (v (if (< h 16) y z))
         (w (if (< h 8) z tt)))
    (+ (if (= (logand h 1) 0) (- u) u)
       (if (= (logand h 2) 0) (- v) v)
       (if (= (logand h 4) 0) (- w) w))))


(defun simplex-noise-1d (x)
  (let* ((i0 (floor x))
         (i1 (+ i0 1))
         (x0 (- x i0))
         (x1 (- x0 1.0))
         (t0 (* (- 1.0 (* x0 x0)) (- 1.0 (* x0 x0))))
         (n0 (* t0 t0 (sngrad1d (aref +pn-perm+ (logand i0 #xff)) x0)))
         (t1 (* (- 1.0 (* x1 x1)) (- 1.0 (* x1 x1))))
         (n1 (* t1 t1 (sngrad1d (aref +pn-perm+ (logand i1 #xff)) x1))))
    ;(* 0.25 (+ n0 n1))))  ; matches PRMan's 1D noise
    (* 0.395 (+ n0 n1))))  ; scales the result to exactly within [-1,1]


(defun simplex-noise-2d (x y)
  (let* ((s (* +f2+ (+ x y)))
         (xs (+ x s))
         (ys (+ y s))
         (i (floor xs))
         (j (floor ys))
         (tt (* +g2+ (+ i j)))
         (x0 (- x (- i tt)))
         (y0 (- y (- j tt)))
         (i1 (if (> x0 y0) 1 0))
         (j1 (if (> x0 y0) 0 1))
         (x1 (+ (- x0 i1) +g2+))
         (y1 (+ (- y0 j1) +g2+))
         (x2 (+ (- x0 1.0) (* 2.0 +g2+)))
         (y2 (+ (- y0 1.0) (* 2.0 +g2+)))
         (ii (mod i 256))
         (jj (mod j 256))
         (t0 (if (< (- 0.5 (* x0 x0) (* y0 y0)) 0.0)
                 (- 0.5 (* x0 x0) (* y0 y0))
                 (* (- 0.5 (* x0 x0) (* y0 y0)) (- 0.5 (* x0 x0) (* y0 y0)))))
         (n0 (if (< (- 0.5 (* x0 x0) (* y0 y0)) 0.0)
                 0.0
                 (* t0 t0 (sngrad2d (aref +pn-perm+ (+ ii (aref +pn-perm+ jj)))
                                    x0 y0))))
         (t1 (if (< (- 0.5 (* x1 x1) (* y1 y1)) 0.0)
                 (- 0.5 (* x1 x1) (* y1 y1))
                 (* (- 0.5 (* x1 x1) (* y1 y1)) (- 0.5 (* x1 x1) (* y1 y1)))))
         (n1 (if (< (- 0.5 (* x1 x1) (* y1 y1)) 0.0)
                 0.0
                 (* t1 t1 (sngrad2d (aref +pn-perm+
                                          (+ ii i1 (aref +pn-perm+ (+ jj j1))))
                                    x1 y1))))
         (t2 (if (< (- 0.5 (* x2 x2) (* y2 y2)) 0.0)
                 (- 0.5 (* x2 x2) (* y2 y2))
                 (* (- 0.5 (* x2 x2) (* y2 y2)) (- 0.5 (* x2 x2) (* y2 y2)))))
         (n2 (if (< (- 0.5 (* x2 x2) (* y2 y2)) 0.0)
                 0.0
                 (* t2 t2 (sngrad2d (aref +pn-perm+ 
                                          (+ ii 1 (aref +pn-perm+ (+ jj 1))))
                                    x2 y2)))))
    (* 40.0 (+ n0 n1 n2))))


(defun simplex-noise-3d (x y z)
  (let* ((s (* +f3+ (+ x y z)))
         (xs (+ x s))
         (ys (+ y s))
         (zs (+ z s))
         (i (floor xs))
         (j (floor ys))
         (k (floor zs))
         (tt (* +g3+ (+ i j k)))
         (x0 (- x (- i tt)))
         (y0 (- y (- j tt)))
         (z0 (- z (- k tt)))
         (i1 (if (>= x0 y0)
                 (if (>= y0 z0) 1 (if (>= x0 z0) 1 0))
                 (if (< y0 z0) 0 (if (< x0 z0) 0 0))))
         (j1 (if (>= x0 y0)
                 (if (>= y0 z0) 0 (if (>= x0 z0) 0 0))
                 (if (< y0 z0) 0 (if (< x0 z0) 1 1))))
         (k1 (if (>= x0 y0)
                 (if (>= y0 z0) 0 (if (>= x0 z0) 0 1))
                 (if (< y0 z0) 1 (if (< x0 z0) 0 0))))
         (i2 (if (>= x0 y0)
                 (if (>= y0 z0) 1 (if (>= x0 z0) 1 1))
                 (if (< y0 z0) 0 (if (< x0 z0) 0 1))))
         (j2 (if (>= x0 y0)
                 (if (>= y0 z0) 1 (if (>= x0 z0) 0 0))
                 (if (< y0 z0) 1 (if (< x0 z0) 1 1))))
         (k2 (if (>= x0 y0)
                 (if (>= y0 z0) 0 (if (>= x0 z0) 1 1))
                 (if (< y0 z0) 1 (if (< x0 z0) 1 0))))
         (x1 (+ (- x0 i1) +g3+))
         (y1 (+ (- y0 j1) +g3+))
         (z1 (+ (- z0 k1) +g3+))
         (x2 (+ (- x0 i2) (* 2.0 +g3+)))
         (y2 (+ (- y0 j2) (* 2.0 +g3+)))
         (z2 (+ (- z0 k2) (* 2.0 +g3+)))
         (x3 (+ (- x0 1.0) (* 3.0 +g3+)))
         (y3 (+ (- y0 1.0) (* 3.0 +g3+)))
         (z3 (+ (- z0 1.0) (* 3.0 +g3+)))
         (ii (mod i 256))
         (jj (mod j 256))
         (kk (mod k 256))
         (t0 (if (< (- 0.6 (* x0 x0) (* y0 y0) (* z0 z0)) 0.0)
                 (- 0.6 (* x0 x0) (* y0 y0) (* z0 z0))
                 (*  (- 0.6 (* x0 x0) (* y0 y0) (* z0 z0))
                     (- 0.6 (* x0 x0) (* y0 y0) (* z0 z0)))))
         (n0 (if (< (- 0.6 (* x0 x0) (* y0 y0) (* z0 z0)) 0.0)
                 0.0
                 (* t0 t0 (sngrad3d (aref +pn-perm+ (+ ii (aref +pn-perm+ (+ jj (aref +pn-perm+ kk))))) x0 y0 z0))))
         (t1 (if (< (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1)) 0.0)
                 (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1))
                 (*  (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1))
                     (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1)))))
         (n1 (if (< (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1)) 0.0)
                 0.0
                 (* t1 t1 (sngrad3d (aref +pn-perm+ (+ ii i1 (aref +pn-perm+ (+ jj j1 (aref +pn-perm+ (+ kk k1)))))) x1 y1 z1))))
         (t2 (if (< (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2)) 0.0)
                 (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2))
                 (*  (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2))
                     (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2)))))
         (n2 (if (< (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2)) 0.0)
                 0.0
                 (* t2 t2 (sngrad3d (aref +pn-perm+ (+ ii i2 (aref +pn-perm+ (+ jj j2 (aref +pn-perm+ (+ kk k2)))))) x2 y2 z2))))
         (t3 (if (< (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3)) 0.0)
                 (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3))
                 (*  (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3))
                     (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3)))))
         (n3 (if (< (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3)) 0.0)
                 0.0
                 (* t3 t3 (sngrad3d (aref +pn-perm+ (+ ii 1 (aref +pn-perm+ (+ jj 1 (aref +pn-perm+ (+ kk 1)))))) x3 y3 z3)))))
    (* 32.0 (+ n0 n1 n2 n3))))


;; Test (requires ZPNG)
;(defun test-simplex3 (&key (width 512) (height 512))
;  (labels ((set-blue (image x y value)
;             (setf (aref image y x 2) value))
;           (set-green (image x y value)
;             (setf (aref image y x 1) value))
;           (set-red (image x y value)
;             (setf (aref image y x 0) value))
;           (set-rgb (image x y red green blue)
;             (set-blue image x y blue)
;             (set-green image x y green)
;             (set-red image x y red)))
;    (let* ((png (make-instance 'zpng:png :color-type :truecolor
;                               :width width :height height))
;           (image (zpng:data-array png)))
;      (with-xy (x width y height)
;        (let ((z 0.0)
;              (r 0) (g 0) (b 0)
;              noise)
;          (format t "---~%noise: ~A~%" noise)
;          (setf noise (* (+ (simplex3d (* x 2) (* y 2) z) 1) 20))
;          (format t "noise: ~A~%" noise)
;          (setf noise (floor noise))
;          (format t "noise: ~A~%" noise)
;          (incf noise (* (simplex3d (* x 200) (* y 200) z) 0.5))
;          (format t "noise: ~A~%" noise)
;          (incf noise (* (simplex3d x (* y 100) z) 0.7))
;          (format t "noise: ~A~%" noise)
;          (setf noise (/ (+ noise 1) 2))
;          (format t "noise: ~A~%" noise)
;          (setf r (truncate (* noise 180)))
;          (setf g (truncate (* noise 130)))
;          (setf b (truncate (* noise 80)))
;          (format t "rgb: ~A ~A ~A~%" r g b)
;          (cond ((> r 255) (setf r 255))
;                ((< r 0) (setf r 0)))
;          (cond ((> g 255) (setf g 255))
;                ((< g 0) (setf g 0)))
;          (cond ((> b 255) (setf b 255))
;                ((< b 0) (setf b 0)))
;          (format t "rgb: ~A ~A ~A~%" r g b)
;          (set-rgb image x y r g b)))
;      (zpng:write-png png "tmp.png"))))
