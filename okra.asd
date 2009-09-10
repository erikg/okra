;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; okra.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra-system
  (:use :cl :asdf))

(in-package :okra-system)

(asdf:defsystem :okra
  :version "1.6.2.2"
  :components ((:module src
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "macros")
                             (:file "perlin-noise")
                             (:file "simplex-noise")
                             (:file "noise-functions")
                             (:file "vectors")
                             (:file "okra"))))
  :depends-on (:cffi :okra-bindings :okra-common))
