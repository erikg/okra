;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; noise-functions.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra)


;;;# Bands

(defun bands3d (x y z &key (fn #'perlin-noise) (octaves 8) (multiplier 2))
  (loop with result = 0.0
        with scale = (/ 1.0 multiplier)
        with weight = 0.5
        repeat octaves
        do (incf result
                 (abs (* (funcall fn (* x scale) (* y scale) (* z scale))
                         weight)))
           (setf scale (* scale multiplier))
           (setf weight (/ weight multiplier))
        finally (return (sin (+ x result)))))


;;;# Fractal Brownian Motion

(defun fbm2d (x y &key (fn #'simplex-noise-2d) (octaves 8) (multiplier 2))
  (loop with result = 0.0
        with scale = (/ 1.0 multiplier)
        with weight = 0.5
        repeat octaves
        do (incf result (* (funcall fn (* x scale) (* y scale)) weight))
           (setf scale (* scale multiplier))
           (setf weight (/ weight multiplier))
        finally (return result)))


(defun fbm3d (x y z &key (fn #'perlin-noise) (octaves 8) (multiplier 2))
  (loop with result = 0.0
        with scale = (/ 1.0 multiplier)
        with weight = 0.5
        repeat octaves
        do (incf result (* (funcall fn (* x scale) (* y scale) (* z scale))
                           weight))
           (setf scale (* scale multiplier))
           (setf weight (/ weight multiplier))
        finally (return result)))


;;;# Ridges

(defun ridge3d (x y z &key (fn #'perlin-noise) (octaves 8) (multiplier 2)
                (ridge 0.0))
  (loop with result = 0.0
        with scale = (/ 1.0 multiplier)
        with weight = 0.5
        repeat octaves
        do (incf result (* (funcall fn (* x scale) (* y scale) (* z scale))
                           weight))
           (setf scale (* scale multiplier))
           (setf weight (/ weight multiplier))
        finally (return (if (> result ridge)
                            (- ridge (- result ridge))
                            result))))


;;;# Turbulence

(defun turbulence3d (x y z &key (fn #'perlin-noise) (octaves 8) (multiplier 2))
  (loop with result = 0.0
        with scale = (/ 1.0 multiplier)
        with weight = 0.5
        repeat octaves
        do (incf result
                 (abs (* (funcall fn (* x scale) (* y scale) (* z scale))
                         weight)))
           (setf scale (* scale multiplier))
           (setf weight (/ weight multiplier))
        finally (return result)))
