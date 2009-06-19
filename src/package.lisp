;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra
  (:use :cl :okra-bindings :okra-common)
  ;; choices.. choices..
  ;(:shadowing-import-from :cl :position))
  (:shadowing-import-from :okra-bindings :position))
