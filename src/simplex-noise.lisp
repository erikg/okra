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


(defun sngrad1d-sf (hash x)
  (declare (inline * + - = logand)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type fixnum hash)
           (type single-float x))
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


(defun sngrad2d-sf (hash x y)
  (declare (inline * + - = logand)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type fixnum hash)
           (type single-float x y))
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


;; reference implementation
(defun simplex-noise-1d (x)
  (let* ((i0 (floor x))
         (i1 (+ i0 1))
         (x0 (- x i0))
         (x1 (- x0 1.0))
         (1-x0^2 (- 1.0 (* x0 x0)))
         (t0 (* 1-x0^2 1-x0^2))
         (n0 (* t0 t0 (sngrad1d (svref +pnp+ (logand i0 #xff)) x0)))
         (1-x1^2 (- 1.0 (* x1 x1)))
         (t1 (* 1-x1^2 1-x1^2))
         (n1 (* t1 t1 (sngrad1d (svref +pnp+ (logand i1 #xff)) x1))))
    ;(* 0.25 (+ n0 n1))))  ; matches PRMan's 1D noise
    (* 0.395 (+ n0 n1))))  ; scales the result to exactly within [-1,1]


(defun simplex-noise-1d-single-float (x)
  "SINGLE-FLOAT version of SIMPLEX-NOISE-1D which has less accuracy but is a
  lot faster and generally good enough unless you need the precision."
  (declare ;; Just inline them all :)
           (inline * + - floor logand sngrad1d-sf svref)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float x))
  (let* ((i0 (the fixnum (floor x)))
         (x0 (- x i0))
         (x1 (- x0 1.0))
         (1-x0^2 (- 1.0 (* x0 x0)))
         (t0 (* 1-x0^2 1-x0^2))
         (n0 (* t0 t0
                (sngrad1d-sf (the fixnum (svref +pnp+ (logand i0 #xff))) x0)))
         (1-x1^2 (- 1.0 (* x1 x1)))
         (t1 (* 1-x1^2 1-x1^2))
         (n1 (* t1 t1
                (sngrad1d-sf (the fixnum (svref +pnp+ (logand (+ i0 1) #xff)))
                             x1))))
    (* 0.395 (+ n0 n1))))  ; scales the result to exactly within [-1,1]

(defalias #'simplex-noise-1d-single-float 'simplex-noise-1d-sf)


;; reference implementation
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
                 (* t0 t0
                    (sngrad2d (svref +pnp+ (+ ii (svref +pnp+ jj))) x0 y0))))
         (t1 (if (< (- 0.5 (* x1 x1) (* y1 y1)) 0.0)
                 (- 0.5 (* x1 x1) (* y1 y1))
                 (* (- 0.5 (* x1 x1) (* y1 y1)) (- 0.5 (* x1 x1) (* y1 y1)))))
         (n1 (if (< (- 0.5 (* x1 x1) (* y1 y1)) 0.0)
                 0.0
                 (* t1 t1
                    (sngrad2d (svref +pnp+ (+ ii i1 (svref +pnp+ (+ jj j1))))
                              x1 y1))))
         (t2 (if (< (- 0.5 (* x2 x2) (* y2 y2)) 0.0)
                 (- 0.5 (* x2 x2) (* y2 y2))
                 (* (- 0.5 (* x2 x2) (* y2 y2)) (- 0.5 (* x2 x2) (* y2 y2)))))
         (n2 (if (< (- 0.5 (* x2 x2) (* y2 y2)) 0.0)
                 0.0
                 (* t2 t2
                    (sngrad2d (svref +pnp+ (+ ii 1 (svref +pnp+ (+ jj 1))))
                              x2 y2)))))
    (* 40.0 (+ n0 n1 n2))))


(defun simplex-noise-2d-single-float (x y)
  "SINGLE-FLOAT version of SIMPLEX-NOISE-2D which has less accuracy but is a
  lot faster and generally good enough unless you need the precision."
  (declare (inline * + - < > floor mod sngrad2d-sf svref)
           (optimize (compilation-speed 0) (debug 0) (safety 0) (space 0)
                     (speed 3))
           (type single-float x y))
  (let* ((s (* +f2+ (+ x y)))
         (i (the fixnum (floor (+ x s))))
         (j (the fixnum (floor (+ y s))))
         (tt (* +g2+ (+ i j)))
         (x0 (- x (- i tt)))
         (y0 (- y (- j tt)))
         (i1 (if (> x0 y0) 1 0))
         (j1 (if (> x0 y0) 0 1))
         (x1 (+ (- x0 i1) +g2+))
         (y1 (+ (- y0 j1) +g2+))
         (x2 (+ (- x0 1.0) (* 2.0 +g2+)))
         (y2 (+ (- y0 1.0) (* 2.0 +g2+)))
         (ii (the fixnum (mod i 256)))
         (jj (the fixnum (mod j 256)))
         (t0 (- 0.5 (* x0 x0) (* y0 y0)))
         (n0 (if (< t0 0.0)
                 0.0
                 (* t0 t0 t0 t0 (sngrad2d-sf (the fixnum (svref +pnp+ (+ ii (the fixnum (svref +pnp+ jj))))) x0 y0))))
         (t1 (- 0.5 (* x1 x1) (* y1 y1)))
         (n1 (if (< t1 0.0)
                 0.0
                 (* t1 t1 t1 t1 (sngrad2d-sf (the fixnum (svref +pnp+ (+ ii i1 (the fixnum (svref +pnp+ (+ jj j1)))))) x1 y1))))
         (t2 (- 0.5 (* x2 x2) (* y2 y2)))
         (n2 (if (< t2 0.0)
                 0.0
                 (* t2 t2 t2 t2 (sngrad2d-sf (the fixnum (svref +pnp+ (+ ii 1 (the fixnum (svref +pnp+ (+ jj 1)))))) x2 y2)))))
    (* 40.0 (+ n0 n1 n2))))

(defalias #'simplex-noise-2d-single-float 'simplex-noise-2d-sf)


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
                 (* t0 t0 (sngrad3d (svref +pnp+ (+ ii (svref +pnp+ (+ jj (svref +pnp+ kk))))) x0 y0 z0))))
         (t1 (if (< (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1)) 0.0)
                 (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1))
                 (*  (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1))
                     (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1)))))
         (n1 (if (< (- 0.6 (* x1 x1) (* y1 y1) (* z1 z1)) 0.0)
                 0.0
                 (* t1 t1 (sngrad3d (svref +pnp+ (+ ii i1 (svref +pnp+ (+ jj j1 (svref +pnp+ (+ kk k1)))))) x1 y1 z1))))
         (t2 (if (< (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2)) 0.0)
                 (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2))
                 (*  (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2))
                     (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2)))))
         (n2 (if (< (- 0.6 (* x2 x2) (* y2 y2) (* z2 z2)) 0.0)
                 0.0
                 (* t2 t2 (sngrad3d (svref +pnp+ (+ ii i2 (svref +pnp+ (+ jj j2 (svref +pnp+ (+ kk k2)))))) x2 y2 z2))))
         (t3 (if (< (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3)) 0.0)
                 (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3))
                 (*  (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3))
                     (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3)))))
         (n3 (if (< (- 0.6 (* x3 x3) (* y3 y3) (* z3 z3)) 0.0)
                 0.0
                 (* t3 t3 (sngrad3d (svref +pnp+ (+ ii 1 (svref +pnp+ (+ jj 1 (svref +pnp+ (+ kk 1)))))) x3 y3 z3)))))
    (* 32.0 (+ n0 n1 n2 n3))))


;;;# Simple Tests

(defun simplex-noise-compare (fn1 fn2 &key (lines 20) (sleep-time 0.0))
  (format t "~16@S <=> ~16S~%------------------+-----------------~%"
          fn1 fn2)
  (loop for xyz from -3.0 to 3.0 by (/ 6.1 lines)
        for i from 0
        do (let ((fn1-result (funcall fn1 xyz xyz))
                 (fn2-result (funcall fn2 xyz xyz)))
             (format t "~16@S <=> ~16S   ~A~%" fn1-result fn2-result
                     (if (equal fn1-result fn2-result)
                         ""
                         "!")))
           (sleep sleep-time)))
