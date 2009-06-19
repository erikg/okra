;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; package.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra-common
  (:use :cl)
  (:export :append1 :asdf :current-date-time-string :error-message :last1
           :mkfstr :mkstr :quit :unique-id :verbose :write-to-file))
