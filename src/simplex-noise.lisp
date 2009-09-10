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


;; from: http://stephencarmody.wikispaces.com/Simplex+Noise
;; I think I just created a horrific Frankenstein of Lisp and obscure Java.
(defun simplex3d (x y z)
  (let (i j k s u v w hi lo
        (ax (vector 0.0 0.0 0.0))
        (tx #(#x15 #x38 #x32 #x2c #x0d #x13 #x07 #x2a)))
    (labels ((b (ii jj kk bb)
               (elt tx (logior (ash (bx ii bb) 2) (ash (bx jj bb) 1)
                               (bx kk bb))))
             (bx (nn bb)
               (logand (ash (truncate nn) (- bb)) 1))
             (k (a)
               (setf s (* (+ (elt ax 0) (elt ax 1) (elt ax 2)) 0.166666667))
               (let* ((x (- u (+ (elt ax 0) s)))
                      (y (- v (+ (elt ax 1) s)))
                      (z (- w (+ (elt ax 2) s)))
                      (tx (- 0.6 (* x x) (* y y) (* z z)))
                      (h (shuffle (+ i (elt ax 0)) (+ j (elt ax 1))
                                  (+ k (elt ax 2)))))
                 (incf (elt ax a))
                 (if (< tx 0)
                     0.0
                     (let* ((b5 (logand (ash h -5) 1))
                            (b4 (logand (ash h -4) 1))
                            (b3 (logand (ash h -3) 1))
                            (b2 (logand (ash h -2) 1))
                            (b1 (logand h 3))
                            (p (if (= b1 1)
                                   x
                                   (if (= b1 2) y z)))
                            (q (if (= b1 1)
                                   y
                                   (if (= b1 2) z x)))
                            (r (if (= b1 1)
                                   z
                                   (if (= b1 2) x y))))
                       (when (= b5 b3)
                         (setf p (- p)))
                       (when (= b5 b4)
                         (setf q (- q)))
                       (when (not (= b5 (logxor b4 b3)))
                         (setf r (- r)))
                       (setf tx (* tx tx))
                       (* 8 tx tx (+ p (if (= b1 0)
                                           (+ q r)
                                           (if (= b2 0) q r))))))))
             (shuffle (ii jj kk)
               (+ (b ii jj kk 0) (b jj kk ii 1) (b kk ii jj 2) (b ii jj kk 3)
                  (b jj kk ii 4) (b kk ii jj 5) (b ii jj kk 6) (b jj kk ii 7))))
      (setf s (* (+ x y z) 0.333333333))
      (setf i (floor (+ x s)))  ; try (mod (+ x s) 1) later for speed
      (setf j (floor (+ y s)))
      (setf k (floor (+ z s)))
      (setf s (* (+ i j k) 0.166666667))
      (setf u (- x (+ i s)))
      (setf v (- y (+ j s)))
      (setf w (- z (+ k s)))
      (setf hi (if (>= u w)
                   (if (>= u v) 0 1)
                   (if (>= v w) 1 2)))
      (setf lo (if (< u w)
                   (if (< u v) 0 1)
                   (if (< v w) 1 2)))
      (+ (k hi) (k (- 3 hi lo)) (k lo) (k 0)))))


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
