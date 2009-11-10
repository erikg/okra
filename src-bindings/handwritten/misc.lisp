;;;; misc.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings)


;;; Foreign Functions & Methods

(defcfun "hw_add_viewport"
    :pointer
  (render-window :pointer)
  (camera :pointer))


;; Not used but kept around for reference & later experiments.
(defcfun ("hw_conan" conan)
    :boolean
  (array-shuttle :pointer))


;; Used for GTK experiments.
(defcfun "hw_create_render_window"
    :pointer
  (ogre-root :pointer)
  (name :string)
  (width :unsigned-int)
  (height :unsigned-int)
  (full-screen :boolean)
  (parent-window-handle :string))


(defcfun ("hw_get_overlay_manager_singleton_ptr"
          get-overlay-manager-singleton-ptr)
    :pointer)


(defcfun ("hw_get_resource_group_manager_singleton_ptr"
          get-resource-group-manager-singleton-ptr)
    :pointer)


(defcfun ("hw_get_window_handler" get-window-handler)
    :unsigned-long
  (render-window :pointer))


(defcfun ("hw_message_pump" message-pump)
    :void)


(defcfun "hw_root_constructor"
    :pointer
  (plugin :string)
  (config :string)
  (log :string))

(defgeneric root-constructor (this &key plugin config log))

(defmethod root-constructor ((this root) &key (plugin "") (config "") (log ""))
  (if (pointer-to this)
      (pointer-to this)
      (setf (slot-value this 'pointer)
            (hw-root-constructor plugin config log))))
