;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cegui.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

;; for development
;#+sbcl (setf *muffled-warnings* 'implicit-generic-function-warning)


;;; Packages

(asdf :okra)
(asdf :okra-cegui)
(asdf :clois-lane-cegui)  ; after okra-cegui to surpress warnings

(in-package :okra)


;;; Parameters

(defparameter *camera* nil)
(defparameter *cube* nil)
(defparameter *light* nil)
(defparameter *plane* nil)
(defparameter *sphere* nil)
(defparameter *viewport* nil)

;; for CEGUI
(defparameter *cegui-system* nil)
(defparameter *cegui-renderer* nil)

;; for clois-lane
(defvar *actions*
  '(;(:key-default . echo-self)
    ;(:mouse-button-default . echo-self)
    ;(:mouse-move-default . echo-self)
    (:kc-escape . stop-running)   (:kc-w . toggle-wireframe)
    (:kc-pgdown . move-down)      (:kc-v . move-down)
    (:kc-pgup   . move-up)        (:kc-r . move-up)
    (:kc-up     . move-forward)   (:kc-e . move-forward)
    (:kc-down   . move-backward)  (:kc-d . move-backward)
    (:kc-left   . move-left)      (:kc-s . move-left)
    (:kc-right  . move-right)     (:kc-f . move-right)
    (:mouse-x   . camera-x-look)  (:mouse-y . camera-y-look)
    (:mouse-button-1 . toggle-mouse-look)))

(defparameter *input-system* nil)
(defparameter *mouse-look* nil)
(defparameter *move-backward* nil)
(defparameter *move-forward* nil)
(defparameter *move-left* nil)
(defparameter *move-right* nil)
(defparameter *move-down* nil)
(defparameter *move-up* nil)
(defparameter *running* nil)


;;; Key Actions

(let ((last-x 0))
  (defun camera-x-look (axis rel-x abs-x)
    (declare (ignore axis abs-x))
    (when *mouse-look*
      ;; this smooths the mouse movement a little
      (yaw *camera* (* (/ (+ rel-x last-x) 2.0) -0.01))
      (setf last-x rel-x))))


(let ((last-y 0))
  (defun camera-y-look (axis rel-y abs-y)
    (declare (ignore axis abs-y))
    (when *mouse-look*
      ;; this smooths the mouse movement a little
      (pitch *camera* (* (/ (+ rel-y last-y) 2.0) 0.01))
      (setf last-y rel-y))))


;; "(and *move-dir* (not *move-opposite*))" is for when both keys are pressed
(defun do-movement ()
  (let ((cd (get-derived-direction *camera*))
        (cp (get-derived-position *camera*))
        (cr (get-derived-right *camera*))
        (moved nil)
        (new-position (vector 0.0 0.0 0.0)))
    ;; I need to add a vector multiply function to src/vectors.lisp.
    (setf (svref cd 0) (* (svref cd 0) 1.5))
    (setf (svref cd 1) (* (svref cd 1) 1.5))
    (setf (svref cd 2) (* (svref cd 2) 1.5))
    (setf (svref cr 0) (* (svref cr 0) 1.5))
    (setf (svref cr 1) (* (svref cr 1) 1.5))
    (setf (svref cr 2) (* (svref cr 2) 1.5))
    (when (and *move-backward* (not *move-forward*))
      (setf new-position (vector-substract new-position cd))
      (setf moved t))
    (when (and *move-down* (not *move-up*))
      (setf new-position (vector-substract new-position #(0 1 0)))
      (setf moved t))
    (when (and *move-forward* (not *move-backward*))
      (setf new-position (vector-add new-position cd))
      (setf moved t))
    (when (and *move-left* (not *move-right*))
      (setf new-position (vector-substract new-position cr))
      (setf moved t))
    (when (and *move-right* (not *move-left*))
      (setf new-position (vector-add new-position cr))
      (setf moved t))
    (when (and *move-up* (not *move-down*))
      (setf new-position (vector-add new-position #(0 1 0)))
      (setf moved t))
    (when moved
      (set-position *camera* (vector-add cp new-position)))))


;; for debugging, mainly
(defun echo-self (&rest args)
  (format t "~&[echo-self] ~S~%" args))


(defun move-backward (key text state)
  (declare (ignore key text))
  (if (equal state :pressed)
      (setf *move-backward* t)
      (setf *move-backward* nil)))


(defun move-down (key text state)
  (declare (ignore key text))
  (if (equal state :pressed)
      (setf *move-down* t)
      (setf *move-down* nil)))


(defun move-forward (key text state)
  (declare (ignore key text))
  (if (equal state :pressed)
      (setf *move-forward* t)
      (setf *move-forward* nil)))


(defun move-left (key text state)
  (declare (ignore key text))
  (if (equal state :pressed)
      (setf *move-left* t)
      (setf *move-left* nil)))


(defun move-right (key text state)
  (declare (ignore key text))
  (if (equal state :pressed)
      (setf *move-right* t)
      (setf *move-right* nil)))


(defun move-up (key text state)
  (declare (ignore key text))
  (if (equal state :pressed)
      (setf *move-up* t)
      (setf *move-up* nil)))


(defun stop-running (key text state)
  (declare (ignore key text))
  (when (equal state :released)
    (setf *running* nil)))


(defun toggle-mouse-look (button state)
  (declare (ignore button))
  (if (equal state :pressed)
      (setf *mouse-look* t)
      (setf *mouse-look* nil)))


(let ((pm-mode :pm-solid))
  (defun toggle-wireframe (key text state)
    (declare (ignore key text))
    (when (equal state :released)
      (if (equal pm-mode :pm-solid)
          (progn (set-polygon-mode *camera* :pm-wireframe)
                 (setf pm-mode :pm-wireframe))
          (progn (set-polygon-mode *camera* :pm-solid)
                 (setf pm-mode :pm-solid))))))


;;; Main Program

(setf *render-window*
      (okra-window :render-system #-windows "OpenGL Rendering Subsystem"
                                  #+windows "Direct3D9 Rendering Subsystem"
                   :resources '(("resources" "FileSystem" "General")
                                ("resources/gui" "FileSystem" "General"))))
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


;;; CEGUI

(setf *cegui-renderer*
      (okra-cegui::create-renderer (pointer-to *render-window*)
                                   (pointer-to *scene-manager*)))
(setf *cegui-system* (okra-cegui::create-system *cegui-renderer*))

(okra-cegui::load-scheme "AquaLookSkin.scheme")
(okra-cegui::set-default-mouse-cursor "AquaLook" "MouseArrow")
;(okra-cegui::set-default-font "BlueHighway-12")
(okra-cegui::mouse-cursor-set-image (okra-cegui::get-default-mouse-cursor))

(okra-cegui::inject-mouse-position (/ (get-actual-width *viewport*) 2.0)
                                   (/ (get-actual-height *viewport*) 2.0))

;(defparameter *cegui-sheet* (okra-cegui::load-window-layout "_preview.layout"))
(defparameter *cegui-sheet* (okra-cegui::load-window-layout "test.layout"))
(okra-cegui::set-gui-sheet *cegui-sheet*)

;; see: http://www.cegui.org.uk/wiki/index.php/EventLookup
(okra-cegui::subscribe-event (okra-cegui::get-window "testButton") "Clicked")
(okra-cegui::subscribe-event (okra-cegui::get-window "testWindow")
                             "CloseClicked")

;;; clois-lane

(let ((wh (get-window-handler (pointer-to *render-window*))))
  (setf *input-system* (clois-lane:create-input-system (mkstr wh)
                                                       :hide-mouse t)))
(clois-lane:set-actions *actions*)
(setf *running* t)

;;; main loop

(loop with sleep-time = 0.02
      while *running*
      do (clois-lane:set-window-extents (get-actual-width *viewport*)
                                        (get-actual-height *viewport*))
         (clois-lane:capture)
         (do-movement)
         (new-frame)
         (sleep sleep-time))

(quit)
