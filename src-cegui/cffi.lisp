;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cffi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings)


;;;# Variables

(defparameter *cegui-actions* nil)


;;;# CEGUI C++ Library

(define-foreign-library libcegui
  (:windows "libcegui.dll")
  (:unix "libcegui.so")
  (t "libcegui"))

;; Bit of a change from LOAD-FOREIGN-LIBRARIES.  I have to think about how
;; to do this in a nice, clean way.
(defun load-libcegui ()
  (use-foreign-library libcegui)
  (format t "~&[okra-cegui] foreign library libcegui loaded~%"))

(load-libcegui)


;;;# CEGUI Callbacks

(defcallback on-event
    :void
  ((window-name :string)
   (event-name :string))
  (cond ((assoc (cons window-name event-name) *cegui-actions* :test #'equal)
         (funcall (cdr (assoc (cons window-name event-name) *cegui-actions*
                              :test #'equal))
                  window-name event-name))
        ((assoc :default *cegui-actions*)
         (funcall (cdr (assoc :default *cegui-actions*))
                  window-name event-name))))

(defcvar "clfun_cegui_on_event" :pointer)


(defun initialise-cegui-callbacks ()
  (setf *clfun-cegui-on-event* (get-callback 'on-event)))

(initialise-cegui-callbacks)
