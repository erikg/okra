;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)


(defpackage :okra-mygui
  (:use :cl :cffi :okra :okra-common)
  (:export :initialise-mygui))
