;;;; misc.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings)


;;; Foreign Functions & Methods

(defcfun ("hw_conan" conan)
    :boolean
  (array-shuttle :pointer))


(defcfun "hw_free"
    :void
  (memory :pointer))


(defcfun "hw_get_available_renderers"
    okra-array
  (ogre-root :pointer))


(defcfun ("hw_get_window_handler" get-window-handler)
    :unsigned-long
  (render-window :pointer))


(defcfun ("hw_get_overlay_manager_singleton_ptr"
          get-overlay-manager-singleton-ptr)
    :pointer)


(defcfun ("hw_get_resource_group_manager_singleton_ptr"
          get-resource-group-manager-singleton-ptr)
    :pointer)


;; Allocates SIZE bytes on the C heap which have to be freed with HW-FREE.
(defcfun "hw_malloc"
    :pointer
  (size :unsigned-int))


(defcfun "hw_manual_object"
    :pointer
  (scene-manager :pointer)
  (name :string)
  (material :string)
  (floats-array :pointer)
  (elements :int))


(defcfun "hw_manual_object_triangle"
    :void
  (manual-object :pointer)
  (floats-array :pointer))


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


(defcfun ("hw_simple_add_viewport" simple-add-viewport)
    :pointer
  (render-window :pointer)
  (camera :pointer))


(defcfun ("hw_message_pump" message-pump)
    :void)


(defcfun "free"
    :void
  (memory :pointer))


;; Allocates SIZE bytes on the C heap which have to be freed with HW-FREE.
(defcfun "malloc"
    :pointer
  (size :unsigned-int))
