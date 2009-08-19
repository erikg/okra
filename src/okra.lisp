;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; okra.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra)


;;;# Variables

(defvar *pi* 3.14159265358979323846)
(defvar *pi/180* (/ *pi* 180.0))

(defparameter *render-window* nil)
(defparameter *root-node* nil)
(defparameter *scene-manager* nil)


;;;# Macros

(defmacro with-xy ((x-var x-max y-var y-max &key (x-min 0) (y-min 0))
                   &body body)
  `(loop for ,y-var from ,y-min below ,y-max
         do (loop for ,x-var from ,x-min below ,x-max
                  do ,@body)))


(defmacro with-xyz ((x-var x-max y-var y-max z-var z-max &key (x-min 0)
                     (y-min 0) (z-min 0)) &body body)
  `(loop for ,z-var from ,z-min below ,z-max
         do (loop for ,y-var from ,y-min below ,y-max
                  do (loop for ,x-var from ,x-min below ,x-max
                           do ,@body))))


(defmacro with-xzy ((x-var x-max y-var y-max z-var z-max &key (x-min 0)
                     (y-min 0) (z-min 0)) &body body)
  `(loop for ,y-var from ,y-min below ,y-max
         do (loop for ,z-var from ,z-min below ,z-max
                  do (loop for ,x-var from ,x-min below ,x-max
                           do ,@body))))


;;;# Functions
;;;
;;; I should do something with (scene-manager-create-entity ... ... "Prefab_*")

(defun attach-object-okra (node-or-object &optional (object nil))
  (if object
      (okra-bindings::attach-object node-or-object (pointer-to object))
      (okra-bindings::attach-object *root-node* (pointer-to node-or-object))))


(defun make-camera (&key (look-at #(0.0 0.0 0.0)) (name nil)
                    (near-clip-distance 1.0) (position #(0.0 0.0 -10.0))
                    (scene-manager *scene-manager*))
  (let* ((name (if name name (mkstr "camera-" (unique-id))))
         (camera (make-instance 'camera
                                :pointer (create-camera scene-manager name))))
    (set-near-clip-distance camera near-clip-distance)
    (set-position camera position)
    (look-at camera look-at)
    camera))


(defun make-child-scene-node (&key (name nil) (node *root-node*)
                              (translate #(0.0 0.0 0.0))
                              (rotate #(1.0 0.0 0.0 0.0)))
  (let ((name (if name name (mkstr "child-scene-node-" (unique-id)))))
    (make-instance 'scene-node
                   :pointer (create-child-scene-node node name translate
                                                     rotate))))


(defun make-entity (&key (name nil) (scene-manager *scene-manager*)
                    (prefab-type nil))
  (let ((name (if name name (mkstr "entity-" (unique-id)))))
    (make-instance 'entity
                   :pointer (create-entity scene-manager name prefab-type))))


(defun make-light (&key (diffuse-colour #(1.0 1.0 1.0 1.0))
                   (direction #(0.408 -0.816 0.408))
                   (name nil) (position #(0.0 10.0 0.0))
                   (scene-manager *scene-manager*)
                   (specular-colour #(1.0 1.0 1.0 1.0)) (type :lt-directional))
  (let* ((name (if name name (mkstr "light-" (unique-id))))
         (light (make-instance 'light
                               :pointer (create-light scene-manager name))))
    (set-type light type)
    (set-position light position)
    (set-direction light direction)
    (set-diffuse-colour light diffuse-colour)
    (set-specular-colour light specular-colour)
    light))


(defun make-manual-object (&key (name nil) (scene-manager *scene-manager*))
  (let ((name (if name name (mkstr "manual-object-" (unique-id)))))
    (make-instance 'manual-object
                   :pointer (create-manual-object scene-manager name))))


(defun make-overlay-manager ()
  (make-instance 'overlay-manager
                 :pointer (get-overlay-manager-singleton-ptr)))


(defun make-render-window (&key (name nil) (width 800) (height 600)
                           (fullscreen nil) (misc-params (cffi:null-pointer)))
  (let ((name (if name name (mkstr "render-window-" (unique-id)))))
    (make-instance 'render-window :pointer
                   (create-render-window *ogre-root* name width height
                                         fullscreen misc-params))))


(defun make-resource-group-manager ()
  (make-instance 'resource-group-manager
                 :pointer (get-resource-group-manager-singleton-ptr)))


(defun make-scene-manager (type &optional (name nil))
  (let ((name (if name name (mkstr "scene-manager-" (unique-id)))))
    (make-instance 'scene-manager
                   :pointer (create-scene-manager *ogre-root* type name))))


(defun make-timer ()
  (make-instance 'timer :pointer (get-timer *ogre-root*)))


;(defun make-viewport (camera &key (render-window *render-window*) (z-order 0)
;                      (left 0.0) (top 0.0) (width 0.0) (height 0.0))
;  (make-instance 'viewport
;                 :pointer (add-viewport render-window (pointer-to camera)
;                                        z-order left top width height)))
;; the direct binding has some 'issues'
(defun make-viewport (camera &key (background-colour '(0.0 0.0 0.0 1.0))
                      (render-window *render-window*))
  (let ((vp (make-instance 'viewport :pointer
                           (simple-add-viewport (pointer-to render-window)
                                                (pointer-to camera)))))
    (set-aspect-ratio camera (* 1.0 (/ (get-actual-width vp)
                                       (get-actual-height vp))))
    (set-background-colour vp background-colour)
    vp))


(defun new-frame ()
  (message-pump)
  (render-one-frame *ogre-root*))


(defun okra-window (&key (name "Okra") (width 800) (height 600)
                    (plugins '("Plugin_CgProgramManager"
                               "Plugin_OctreeSceneManager"))
                    (render-system "OpenGL Rendering Subsystem")
                    (resources '(("resources" "FileSystem" "General"))))
  (setf *ogre-root* (make-instance 'root))
  (root-constructor *ogre-root* :log "Okra.log")
  #+windows (load-plugin *ogre-root* "RenderSystem_Direct3D9")
  (load-plugin *ogre-root* "RenderSystem_GL")
  (if render-system
      (set-render-system *ogre-root*
                         (get-render-system-by-name *ogre-root* render-system))
      ;; otherwise grab the first one available
      (let* ((renderer-name (first (get-available-renderers *ogre-root*)))
             (renderer (get-render-system-by-name *ogre-root* renderer-name)))
        (set-render-system *ogre-root* renderer)))
    (initialise *ogre-root* nil name "")
    (dolist (plugin plugins)
      (load-plugin *ogre-root* plugin))
    ;; the render-window needs to be created before scripts are loaded
    (let ((rgm (make-resource-group-manager))
          (rw (make-render-window :name name :width width :height height)))
      (dolist (resource resources)
        (add-resource-location rgm (first resource) (second resource)
                               (third resource) nil))
      (dolist (resource resources)
        (initialise-resource-group rgm (third resource)))
      rw))


(defun root-node (&optional (scene-manager *scene-manager*))
  (make-instance 'scene-node :pointer (get-root-scene-node scene-manager)))
