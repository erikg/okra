;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cffi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings)  ; XXX: shouldn't this be :okra-mygui?


;;;# Variables

(defparameter *mygui-actions* nil)


;;;# MYGUI C++ Library

(define-foreign-library libmygui
  (:windows "libmygui_okra.dll")
  (:unix "libmygui_okra.so")
  (t "libmygui_okra"))

;; Bit of a change from LOAD-FOREIGN-LIBRARIES.  I have to think about how
;; to do this in a nice, clean way.
(defun load-libmygui ()
  (use-foreign-library libmygui)
  (format t "~&[okra-mygui] foreign library libmygui_okra loaded~%"))

(load-libmygui)


;;;;# MYGUI Callbacks
;
;(defcallback on-event
;    :void
;  ((window-name :string)
;   (event-name :string))
;  (cond ((assoc (cons window-name event-name) *cegui-actions* :test #'equal)
;         (funcall (cdr (assoc (cons window-name event-name) *cegui-actions*
;                              :test #'equal))
;                  window-name event-name))
;        ((assoc :default *cegui-actions*)
;         (funcall (cdr (assoc :default *cegui-actions*))
;                  window-name event-name))))
;
;(defcvar "clfun_cegui_on_event" :pointer)
;
;
;(defun initialise-cegui-callbacks ()
;  (setf *clfun-cegui-on-event* (get-callback 'on-event)))
;
;(initialise-cegui-callbacks)
