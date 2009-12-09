;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; mygui.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-mygui)


;;; Foreign Functions

(defcfun "mygui_constructor"
    :pointer)


(defcfun "mygui_initialise"
    :void
  (mygui :pointer)
  (ogre-render-window :pointer))


(defcfun "mygui_load_layout"
    :void
  (name :string))
