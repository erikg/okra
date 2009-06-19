;;;; handwritten.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings)


;;; Foreign Functions & Methods

(defcfun ("hw_conan" conan)
    :boolean
  (array-shuttle :pointer))


(defcfun "hw_get_available_renderers"
    okra-array
  (ogre-root :pointer))


(defun memtest (&optional (n 75))
  (format t "Creating and destroying ~A times...~%---~%" n)
  (sb-ext:gc :full t)
  (room)
  (format t "---~%")
  (loop repeat n
        do (hw-get-available-renderers (pointer-to *ogre-root*))
           (princ #\.)
           (force-output))
  (sb-ext:gc :full t)
  (format t "~%---~%")
  (room))


(defcfun ("hw_get_window_handler" get-window-handler)
    :unsigned-long
  (render-window :pointer))


(defcfun ("hw_get_overlay_manager_singleton_ptr"
          get-overlay-manager-singleton-ptr)
    :pointer)


(defcfun ("hw_get_resource_group_manager_singleton_ptr"
          get-resource-group-manager-singleton-ptr)
    :pointer)


(defcfun "hw_root_constructor"
    :pointer
  (plugin :string)
  (config :string)
  (log :string))

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
