;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cegui.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-cegui)


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


(defcfun ("cegui_inject_mouse_position" inject-mouse-position)
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


(defcfun ("cegui_set_gui_sheet" set-gui-sheet)
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

;; Thanks to for pkhoung for solving this problem.
(defun load-window-layout (layout)
  (let* ((fpm #+(and sbcl linux) (sb-int:get-floating-point-modes)
              #-(and sbcl linux) nil)
         (traps (remove :divide-by-zero (remove :invalid (getf fpm :traps)))))
    #+(and sbcl linux) (sb-int:set-floating-point-modes :traps traps)
    (let ((window (cegui-load-window-layout layout)))
      #+(and sbcl linux) (apply #'sb-int:set-floating-point-modes fpm)
      window)))
