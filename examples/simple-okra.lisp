;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; simple-okra.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; This is a very simple example using both the tiny Lisp layer as well as
;;;; direct calls to the Ogre bindings.

;;; Packages

(asdf :okra)
(in-package :okra)


;;; Parameters

(defparameter *camera* nil)
(defparameter *cube* nil)
(defparameter *light* nil)
(defparameter *plane* nil)
(defparameter *sphere* nil)
(defparameter *viewport* nil)


;;; Main Program

(setf *render-window*
      (okra-window :render-system #-windows "OpenGL Rendering Subsystem"
                                  #+windows "Direct3D9 Rendering Subsystem"
                   :resources '(("resources" "FileSystem" "General"))))
(setf *scene-manager* (make-scene-manager "OctreeSceneManager"))
(setf *root-node* (root-node))

;; camera
(setf *camera* (make-camera))
(set-position *camera* #(100.0 100.0 100.0))
(look-at *camera* #(0.0 0.0 0.0))
(set-near-clip-distance *camera* 1.0)

;; viewport
(setf *viewport* (make-viewport *camera*))
(set-background-colour *viewport* '(0.0 0.0 0.0 1.0))
(set-aspect-ratio *camera* (* 1.0 (/ (get-actual-width *viewport*)
                                     (get-actual-height *viewport*))))

;; light
(setf *light* (make-light))
(set-type *light* :lt-directional)
(set-position *light* #(0.0 100.0 0.0))
(set-diffuse-colour *light* #(0.9 0.9 0.9 1.0))
(set-specular-colour *light* #(0.9 0.9 0.9 1.0))
(set-direction *light* #(1.0 -1.0 -0.5))

;; misc
(set-ambient-light *scene-manager* #(0.2 0.2 0.2 1.0))
(set-shadow-technique *scene-manager* :shadowtype-none)

;; plane
(let ((entity (make-entity :name "plane" :prefab-type :pt-plane)))
  (setf *plane* (make-child-scene-node))
  (attach-object *plane* (pointer-to entity)))
(set-orientation *plane* (vector (sqrt 0.5) (- 0 (sqrt 0.5)) 0.0 0.0))

;; sphere
(let ((entity (make-entity :name "sphere" :prefab-type :pt-sphere)))
  (set-material-name entity "Test/Red")
  (setf *sphere* (make-child-scene-node))
  (attach-object *sphere* (pointer-to entity)))
(set-scale *sphere* #(0.5 0.5 0.5))
(set-position *sphere* #(0.0 20.0 0.0))

;; cube
(let ((entity (make-entity :name "cube" :prefab-type :pt-cube)))
  (set-material-name entity "Test/Blue80")
  (setf *cube* (make-child-scene-node))
  (attach-object *cube* (pointer-to entity)))
(set-scale *cube* #(0.2 0.2 0.2))
(set-position *cube* #(50.0 20.0 0.0))

;; display
(new-frame)
