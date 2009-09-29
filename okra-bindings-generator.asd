;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; okra-bindings-generator.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra-system
  (:use :cl :asdf))

(in-package :okra-system)

(asdf:defsystem :okra-bindings-generator
  :version "1.6.4.1"
  :components
    ((:module bindings-generator
      :serial t
      :components
        ((:file "package")
         (:file "lxml-util")
         (:file "config")
         (:file "common")
         (:file "common-lisp-config")
         (:file "common-lisp-types")
         (:file "common-lisp")
         (:file "cpp-config")
         (:file "cpp-types")
         (:file "cpp")
         (:file "bindings-generator"))))
  :depends-on (:cl-ppcre :html-template :iterate :okra-common :parse-number
               :s-xml))
