;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; ogre-lib.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings)


;;;# Ogre C++ Library

(define-foreign-library libokra
  (:windows "libokra.dll")
  (:unix "libokra.so")
  (t "libokra"))

(use-foreign-library libokra)
(format t "~&[okra] foreign library libokra loaded~%")


;;;# Variables

(defparameter *ogre-root* nil)
(defparameter *tmp-pointer* nil)  ; for okra-array test


;;;# Classes

;;; This class is at the root of the inheritance tree for Ogre classes.
(defclass ogre-object ()
  ((pointer :reader pointer-to :initarg :pointer :initform nil)))


;;;# Enums

(defcenum okra-types
  :int
  :pointer
  :real
  :string)


;;;# Structs

(defcstruct array-shuttle
  "Used for shuttling dynamicly allocated arrays to CL."
  (length :unsigned-int)
  (type okra-types)
  (int_array :pointer)
  (pointer_array :pointer)
  (real_array :pointer)
  (string_array :pointer))


;;;# Foreign Types

(define-foreign-type okra-array ()
  ()
  (:actual-type array-shuttle)
  (:simple-parser okra-array))


(define-foreign-type okra-array3 ()
  ()
  (:actual-type :pointer)
  (:simple-parser okra-array3))


(define-foreign-type okra-array4 ()
  ()
  (:actual-type :pointer)
  (:simple-parser okra-array4))


(define-foreign-type okra-array6 ()
  ()
  (:actual-type :pointer)
  (:simple-parser okra-array6))


(define-foreign-type okra-array16 ()
  ()
  (:actual-type :pointer)
  (:simple-parser okra-array16))


;; XXX: Should either be a :float or :double, see okra.h.
(define-foreign-type okra-real ()
  ()
  (:actual-type :float)
  (:simple-parser okra-real))


;;;# Foreign Type Translators

(defmethod free-translated-object (pointer (value okra-array3) param)
  (declare (ignore param))
  (foreign-free pointer))


(defmethod free-translated-object (pointer (value okra-array4) param)
  (declare (ignore param))
  (foreign-free pointer))


(defmethod free-translated-object (pointer (value okra-array6) param)
  (declare (ignore param))
  (foreign-free pointer))


(defmethod free-translated-object (pointer (value okra-array16) param)
  (declare (ignore param))
  (foreign-free pointer))


;; XXX: only works for strings for now
(defmethod translate-from-foreign (oa (type okra-array))
  (let ((slength (foreign-slot-value oa 'array-shuttle 'length))
        (stype (foreign-slot-value oa 'array-shuttle 'type))
        (slist (foreign-slot-value oa 'array-shuttle 'string_array)))
    (format t "length: ~S; type: ~S; pointer: ~S~%" slength stype slist)
    (setf *tmp-pointer* oa)
    (loop for i from 0 below slength
          collect (copy-seq (mem-aref slist :string i)))))


(defmethod translate-to-foreign (a3 (type okra-array3))
  (foreign-alloc 'okra-real :initial-contents
                 (list (elt a3 0) (elt a3 1) (elt a3 2))))


(defmethod translate-to-foreign (a4 (type okra-array4))
  (foreign-alloc 'okra-real :initial-contents
               (list (elt a4 0) (elt a4 1) (elt a4 2) (elt a4 3))))


(defmethod translate-to-foreign (a6 (type okra-array6))
  (foreign-alloc 'okra-real :initial-contents
                 (list (elt a6 0) (elt a6 1) (elt a6 2)
                       (elt a6 3) (elt a6 4) (elt a6 5))))


(defmethod translate-to-foreign (a16 (type okra-array16))
  (foreign-alloc 'okra-real :initial-contents
           (list (elt a16  0) (elt a16  1) (elt a16  2) (elt a16  3)
                 (elt a16  4) (elt a16  5) (elt a16  6) (elt a16  7)
                 (elt a16  8) (elt a16  9) (elt a16 10) (elt a16 11)
                 (elt a16 12) (elt a16 13) (elt a16 14) (elt a16 15))))
