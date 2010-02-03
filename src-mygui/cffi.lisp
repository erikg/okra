;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cffi.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-mygui)


;;; Variables

(defparameter *gui* nil)
(defparameter *mygui-actions* nil)

;; XXX: hack!
(defparameter *current-x* 0)
(defparameter *current-y* 0)


;;; MYGUI C++ Library

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


;;; MYGUI Callbacks

(defcallback event-mouse-button-click
    :void
  ((widget :pointer))
  (cond ((assoc (cons (get-name widget) :mouse-button-click)
                *mygui-actions* :test #'equal)
         (funcall (cdr (assoc (cons (get-name widget) :mouse-button-click)
                              *mygui-actions* :test #'equal))
                  widget))
        ((assoc :default *mygui-actions*)
         (funcall (cdr (assoc :default *mygui-actions*))
                  widget))))

(defcvar "clfun_mygui_event_mouse_button_click" :pointer)


(defun initialise-mygui-callbacks ()
  (setf *clfun-mygui-event-mouse-button-click*
        (get-callback 'event-mouse-button-click)))

(initialise-mygui-callbacks)
