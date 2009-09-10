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


;;;# Functions

(defun dot2d (g x y)
  (+ (* (aref g 0) x) (* (aref g 1) y)))


(defun dot3d (g x y z)
  (+ (* (aref g 0) x) (* (aref g 1) y) (* (aref g 2) z)))


(defun dot4d (g x y z w)
  (+ (* (aref g 0) x) (* (aref g 1) y) (* (aref g 2) z) (* (aref g 3) w)))


;; This is an almost literal transcription from the PDF.
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
         (gi0 (mod (svref +perlin-noise-permutation+
                          (+ ii (svref +perlin-noise-permutation+ jj)))
                   12))
         (gi1 (mod (svref +perlin-noise-permutation+
                          (+ ii i1
                             (svref +perlin-noise-permutation+ (+ jj j1))))
                   12))
         (gi2 (mod (svref +perlin-noise-permutation+
                          (+ ii 1 (svref +perlin-noise-permutation+ (+ jj 1))))
                   12))
         (t0 (- 0.5 (* x0 x0) (* y0 y0)))
         (n0 (if (< t0 0)
                 0.0
                 (progn (setf t0 (* t0 t0))
                        (* t0 t0 (dot2d (aref +grad3+ gi0) x0 y0)))))
         (t1 (- 0.5 (* x1 x1) (* y1 y1)))
         (n1 (if (< t1 0)
                 0.0
                 (progn (setf t1 (* t1 t1))
                        (* t1 t1 (dot2d (aref +grad3+ gi1) x1 y1)))))
         (t2 (- 0.5 (* x2 x2) (* y2 y2)))
         (n2 (if (< t2 0)
                 0.0
                 (progn (setf t2 (* t2 t2))
                        (* t2 t2 (dot2d (aref +grad3+ gi2) x2 y2))))))
    (* 70.0 (+ n0 n1 n2))))


;; requires ZPNG
(defun test-simplex2d (&key (width 512) (height 512))
  (labels ((set-blue (image x y value)
             (setf (aref image y x 2) value))
           (set-green (image x y value)
             (setf (aref image y x 1) value))
           (set-red (image x y value)
             (setf (aref image y x 0) value))
           (set-rgb (image x y red green blue)
             (set-blue image x y blue)
             (set-green image x y green)
             (set-red image x y red)))
    (let* ((png (make-instance 'zpng:png :color-type :truecolor
                               :width width :height height))
           (image (zpng:data-array png)))
      (with-xy (x width y height)
        (let (;(rgb (truncate (* (+ (simplex2d-reference x y) 1) 127.0)))
              (rgb (truncate (* (+ (fbm2d (/ x 512) (/ y 512)) 1) 127.0))))
          (set-rgb image x y rgb rgb rgb)))
      (zpng:write-png png "tmp.png"))))
