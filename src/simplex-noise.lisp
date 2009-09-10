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

(defparameter +grad3+ #(#(1 1 0) #(-1  1 0) #(1 -1  0) #(-1 -1  0)
                        #(1 0 1) #(-1  0 1) #(1  0 -1) #(-1  0 -1)
                        #(0 1 1) #( 0 -1 1) #(0  1 -1) #( 0 -1 -1)))

(defparameter +grad4+
  #(#( 0  1 1 1) #( 0  1  1 -1) #( 0  1 -1 1) #( 0  1 -1 -1)
    #( 0 -1 1 1) #( 0 -1  1 -1) #( 0 -1 -1 1) #( 0 -1 -1 -1)
    #( 1  0 1 1) #( 1  0  1 -1) #( 1  0 -1 1) #( 1  0 -1 -1)
    #(-1  0 1 1) #(-1  0  1 -1) #(-1  0 -1 1) #(-1  0 -1 -1)
    #( 1  1 0 1) #( 1  1  0 -1) #( 1 -1  0 1) #( 1 -1  0 -1)
    #(-1  1 0 1) #(-1  1  0 -1) #(-1 -1  0 1) #(-1 -1  0 -1)
    #( 1  1 1 0) #( 1  1 -1  0) #( 1 -1  1 0) #( 1 -1 -1  0)
    #(-1  1 1 0) #(-1  1 -1  0) #(-1 -1  1 0) #(-1 -1 -1  0)))


(defparameter +pn-perm+ +perlin-noise-permutation+)


;;;# Functions

(defun dot2d (g x y)
  (+ (* (aref g 0) x) (* (aref g 1) y)))


(defun dot3d (g x y z)
  (+ (* (aref g 0) x) (* (aref g 1) y) (* (aref g 2) z)))


(defun dot4d (g x y z w)
  (+ (* (aref g 0) x) (* (aref g 1) y) (* (aref g 2) z) (* (aref g 3) w)))


;; This is an almost literal transcription from the PDF.
;;
;; It also doesn't seem to be correct yet, since I don't like the result of
;; running TEST-SIMPLEX2D.
(defun simplex2d-reference (x y)
  (let* ((f2 (* 0.5 (- (sqrt 3.0) 1.0)))
         (s (* (+ x y) f2))
         (i (floor (+ x s)))
         (j (floor (+ y s)))
         (g2 (/ (- 3.0 (sqrt 3.0)) 6.0))
         (tx (* (+ i j) g2))  ; can't use t as symbol name
         (x0x (- i tx))
         (y0x (- j tx))
         (x0 (- x x0x))
         (y0 (- y y0x))
         (i1 (if (> x0 y0) 1 0))
         (j1 (if (> x0 y0) 0 1))
         (x1 (- x0 (+ i1 g2)))
         (y1 (- y0 (+ j1 g2)))
         (x2 (- x0 (+ 1.0 (* 2.0 g2))))
         (y2 (- y0 (+ 1.0 (* 2.0 g2))))
         (ii (logand i 255))
         (jj (logand j 255))
         (gi0 (rem (svref +pn-perm+ (+ ii (svref +pn-perm+ jj))) 12))
         (gi1 (rem (svref +pn-perm+ (+ ii i1 (svref +pn-perm+ (+ jj j1)))) 12))
         (gi2 (rem (svref +pn-perm+ (+ ii 1 (svref +pn-perm+ (+ jj 1)))) 12))
         (t0 (- 0.5 (* x0 x0) (* y0 y0)))
         (n0 (if (< t0 0)
                 0.0
                 (let ((t0 (* t0 t0)))
                   (* t0 t0 (dot2d (aref +grad3+ gi0) x0 y0)))))
         (t1 (- 0.5 (* x1 x1) (* y1 y1)))
         (n1 (if (< t1 0)
                 0.0
                 (let ((t1 (* t1 t1)))
                   (* t1 t1 (dot2d (aref +grad3+ gi1) x1 y1)))))
         (t2 (- 0.5 (* x2 x2) (* y2 y2)))
         (n2 (if (< t2 0)
                 0.0
                 (let ((t2 (* t2 t2)))
                   (* t2 t2 (dot2d (aref +grad3+ gi2) x2 y2))))))
    (* 70.0 (+ n0 n1 n2))))
