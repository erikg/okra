;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; config.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra)


;;;# Variables

(defvar *pi* 3.14159265358979323846)
(defvar *pi/180* (/ *pi* 180.0))

(defparameter *render-window* nil)
(defparameter *root-node* nil)
(defparameter *scene-manager* nil)
