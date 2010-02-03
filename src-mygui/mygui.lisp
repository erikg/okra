;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; mygui.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-mygui)


;;; Foreign Functions

(defcfun "mygui_find_widget"
    :pointer
  (name :string)
  (throw-exception :boolean))


(defcfun ("mygui_get_caption" get-caption)
    :string
  (widget :pointer))


(defcfun ("mygui_get_name" get-name)
    :string
  (widget :pointer))


(defcfun ("mygui_initialise" mygui-initialise-cfun)
    :pointer
  (ogre-render-window :pointer)
  (ogre-scene-manager :pointer))


(defcfun "mygui_inject_key_press"
    :boolean
  (gui :pointer)
  (key-code :unsigned-int)
  (code-point :uint32))


(defcfun "mygui_inject_key_release"
    :boolean
  (gui :pointer)
  (key-code :unsigned-int))


(defcfun "mygui_inject_mouse_move"
    :boolean
  (gui :pointer)
  (absolute-x :int)
  (absolute-y :int)
  (absolute-z :int))


(defcfun "mygui_inject_mouse_press"
    :boolean
  (gui :pointer)
  (absolute-x :int)
  (absolute-y :int)
  (id :int))


(defcfun "mygui_inject_mouse_release"
    :boolean
  (gui :pointer)
  (absolute-x :int)
  (absolute-y :int)
  (id :int))


(defcfun "mygui_load_layout"
    :void
  (name :string))


(defcfun ("mygui_set_caption" set-caption)
    :void
  (widget :pointer)
  (text :string))


(defcfun ("mygui_set_event_mouse_button_click" set-event-mouse-button-click)
    :void
  (widget :pointer))


;;; Wrappers

(defun find-widget (name)
  (mygui-find-widget name nil))


(defun inject-key-press (key-code code-point)
  (if *gui*
      (mygui-inject-key-press *gui* key-code code-point)
      (error "[okra-mygui] Please call #'mygui-initialise first.")))


(defun inject-key-release (key-code)
  (if *gui*
      (mygui-inject-key-release *gui* key-code)
      (error "[okra-mygui] Please call #'mygui-initialise first.")))


(defun inject-mouse-move (absolute-x absolute-y absolute-z)
  (if *gui*
      (progn
        (mygui-inject-mouse-move *gui* absolute-x absolute-y absolute-z)
        (setf *current-x* absolute-x)
        (setf *current-y* absolute-y))
      (error "[okra-mygui] Please call #'mygui-initialise first.")))


(defun inject-mouse-press (absolute-x absolute-y id)
  (declare (ignore absolute-x absolute-y))  ; XXX: hack!
  (if *gui*
      (mygui-inject-mouse-press *gui* *current-x* *current-y* id)
      (error "[okra-mygui] Please call #'mygui-initialise first.")))


(defun inject-mouse-release (absolute-x absolute-y id)
  (declare (ignore absolute-x absolute-y))  ; XXX: hack!
  (if *gui*
      (mygui-inject-mouse-release *gui* *current-x* *current-y* id)
      (error "[okra-mygui] Please call #'mygui-initialise first.")))


(defun mygui-initialise (ogre-render-window ogre-scene-manager)
  (setf *gui* (mygui-initialise-cfun ogre-render-window ogre-scene-manager)))
