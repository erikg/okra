;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; okra-common.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra-system
  (:use :cl :asdf))

(in-package :okra-system)

(asdf:defsystem :okra-common
  :version "1.6.4.1"
  :components
    ((:module src-common
      :components
        ((:file "package")
         (:file "common" :depends-on ("package"))))))
