;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cegui.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-cegui)


;;; Foreign Functions

(defcfun ("cegui_create_font" create-font)
    :void
  (font :string))


(defcfun ("cegui_create_renderer" create-renderer)
    :pointer
  (render-window :pointer)
  (scene-manager :pointer))


(defcfun ("cegui_create_system" create-system)
    :pointer
  (ogre-cegui-renderer :pointer))


(defcfun ("cegui_get_default_mouse_cursor" get-default-mouse-cursor)
    :pointer)


(defcfun ("cegui_get_window" get-window)
    :pointer
  (name :string))


(defcfun ("cegui_inject_char" inject-char)
    :void
  (code-point :uint32))


(defcfun ("cegui_inject_key_down" inject-key-down)
    :void
  (key-code :unsigned-int))


(defcfun ("cegui_inject_key_up" inject-key-up)
    :void
  (key-code :unsigned-int))


(defcfun ("cegui_inject_mouse_button_down" inject-mouse-button-down)
    :void
  (mouse-button :int))


(defcfun ("cegui_inject_mouse_button_up" inject-mouse-button-up)
    :void
  (mouse-button :int))


(defcfun ("cegui_inject_mouse_move" inject-mouse-move)
    :void
  (dx :float)
  (dy :float))


(defcfun ("cegui_inject_mouse_position" cegui-inject-mouse-position)
    :void
  (x :float)
  (y :float))


(defcfun ("cegui_load_scheme" load-scheme)
    :pointer
  (scheme :string))


(defcfun ("cegui_load_window_layout" cegui-load-window-layout)
    :pointer
  (layout :string))


(defcfun ("cegui_mouse_cursor_set_image" mouse-cursor-set-image)
    :void
  (image :pointer))


(defcfun ("cegui_render_gui" render-gui)
    :void)


(defcfun ("cegui_set_default_font" set-default-font)
    :void
  (font :string))


(defcfun ("cegui_set_default_mouse_cursor" set-default-mouse-cursor)
    :void
  (look :string)
  (arrow :string))


(defcfun ("cegui_set_gui_sheet" cegui-set-gui-sheet)
    :void
  (sheet :pointer))


(defcfun ("cegui_set_text" set-text)
    :void
  (window :pointer)
  (text :string))


;; see: http://www.cegui.org.uk/wiki/index.php/EventLookup for event names
(defcfun ("cegui_subscribe_event" subscribe-event)
    :void
  (window :pointer)
  (event :string))


;;; Wrappers
;;;
;;; Thanks to for pkhoung for solving this problem.

(defun inject-mouse-position (x y)
  #+sbcl (sb-int:with-float-traps-masked (:divide-by-zero :invalid)
           (cegui-inject-mouse-position x y))
  #-sbcl (cegui-inject-mouse-position x y))


(defun load-window-layout (layout)
  #+sbcl (sb-int:with-float-traps-masked (:divide-by-zero :invalid)
           (cegui-load-window-layout layout))
  #-sbcl (cegui-load-window-layout layout))


(defun set-gui-sheet (sheet)
  #+sbcl (sb-int:with-float-traps-masked (:divide-by-zero :invalid)
           (cegui-set-gui-sheet sheet))
  #-sbcl (cegui-set-gui-sheet sheet))


;;; Functions

;; An improvement over the mess it was before but not quite finished yet.
(defun initialise-cegui (scheme scene &key actions default-font
                         default-mouse-arrow events (layout "gui.layout"))
  (create-system (create-renderer (okra::pointer-to (okra::window-of scene))
                                  (okra::pointer-to (okra::manager-of scene))))
  (load-scheme scheme)
  (when default-font
    (set-default-font default-font))
  (when default-mouse-arrow
    (set-default-mouse-cursor default-mouse-arrow "MouseArrow")
    (mouse-cursor-set-image (get-default-mouse-cursor)))
  (let ((vp (first (okra::viewports-of scene))))
    (inject-mouse-position (/ (okra::get-actual-width vp) 2.0)
                           (/ (okra::get-actual-height vp) 2.0)))
  (set-gui-sheet (load-window-layout layout))
  (when actions
    (setf okra-bindings::*cegui-actions* actions))
  ;; see: http://www.cegui.org.uk/wiki/index.php/EventLookup
  (dolist (event events)
    (subscribe-event (get-window (car event)) (cdr event))))
