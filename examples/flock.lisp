;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; flock.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

;;; Packages

(asdf :okra-cegui)
(asdf :clois-lane-cegui)  ; after okra-cegui to surpress warnings

(in-package :okra)


;;; Parameters

(defparameter *water-grid-size* 10.0)
(defparameter *water-position* 0.0)
(defparameter *water-speed* 0.005)

(defparameter *camera* nil)
(defparameter *cube* nil)
(defparameter *light* nil)
(defparameter *plane* nil)
(defparameter *sphere* nil)
(defparameter *timer* nil)
(defparameter *viewport* nil)
(defparameter *water-mo* nil)  ; manual object
(defparameter *water-node* nil)

;; for CEGUI
(defvar *cegui-actions*
  '((("quitButton" . "Clicked") . cegui-stop-running)
    (:default . echo-self)))

(defparameter *cegui-sheet* nil)
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


;;; CEGUI Actions

(defun cegui-stop-running (window-name event-name)
  (declare (ignore window-name event-name))
  (setf *running* nil))


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


;;; Functions

(defun dy (x y z width)
  (* 25.0 (perlin-noise (/ x width) (+ y *water-position*) (/ z width))))


(defun water-surface-loop (manual-object x y z width grid-size)
  (loop with prev-dyxz = (make-array (+ 1 (ceiling (/ width grid-size)))
                                     :initial-element nil)
        with mo = manual-object
        with w/2 = (/ width 2.0)
        with x-max = (+ x w/2)
        with x-min = (- x w/2)
        with z-max = (+ z w/2)
        with z-min = (- z w/2)
        for z from z-min below z-max by grid-size
        for z+ = (+ z grid-size)
        do (loop for x from x-min below x-max by grid-size
                 for i from 0
                 for x+ = (+ x grid-size)
                 for dyxz = (if (aref prev-dyxz i)
                                (aref prev-dyxz i)
                                (dy x y z width))
                 for dyx+z = (if (aref prev-dyxz (+ i 1))
                                 (aref prev-dyxz (+ i 1))
                                 (dy x+ y z width))
                 for dyxz+ = (dy x y z+ width)
                 for dyx+z+ = (dy x+ y z+ width)
                 ;; fvn = face normal
                 for fn1 = (vector-normal (xyz2v x dyxz z x+ dyx+z+ z+)
                                          (xyz2v x dyxz z x+ dyx+z z))
                 for fn2 = (vector-normal (xyz2v x dyxz z x dyxz+ z+)
                                          (xyz2v x dyxz z x+ dyx+z+ z+))
                 do (setf (aref prev-dyxz i) dyxz+)
                    ;; 1st grid triangle
                    (position mo x dyxz z)
                    ;(normal mo fn1)  ; passing a vector is slow as shit!
                    (normal mo (elt fn1 0) (elt fn1 1) (elt fn1 2))
                    (position mo x+ dyx+z+ z+)
                    (normal mo (elt fn1 0) (elt fn1 1) (elt fn1 2))
                    (position mo x+ dyx+z z)
                    (normal mo (elt fn1 0) (elt fn1 1) (elt fn1 2))
                    ;; 2nd grid triangle
                    (position mo x dyxz z)
                    (normal mo (elt fn2 0) (elt fn2 1) (elt fn2 2))
                    (position mo x dyxz+ z+)
                    (normal mo (elt fn2 0) (elt fn2 1) (elt fn2 2))
                    (position mo x+ dyx+z+ z+)
                    (normal mo (elt fn2 0) (elt fn2 1) (elt fn2 2))
                 finally (setf (aref prev-dyxz (+ i 1)) dyx+z+))))


(defun create-water-surface (x y z width &key (grid-size 4.0)
                             (material "BaseWhiteNoLighting"))
  (let* ((mo (make-manual-object)))
    (set-dynamic mo t)
    (begin mo material :ot-triangle-list)
    (water-surface-loop mo x y z width grid-size)
    (end mo)
    (incf *water-position* *water-speed*)
    mo))


(defun update-water-surface (manual-object x y z width &key (grid-size 4.0))
  (begin-update manual-object 0)
  (water-surface-loop manual-object x y z width grid-size)
  (end manual-object)
  (incf *water-position* *water-speed*))


;;; Initialisation

(defun initialise-application ()
  (setf *render-window*
        (okra-window :render-system #-windows "OpenGL Rendering Subsystem"
                                    #+windows "Direct3D9 Rendering Subsystem"
                     :resources
                     '(("resources" "FileSystem" "General")
                       ("resources/gui/configs" "FileSystem" "General")
                       ("resources/gui/fonts" "FileSystem" "General")
                       ("resources/gui/imagesets" "FileSystem" "General")
                       ("resources/gui/layouts" "FileSystem" "General")
                       ("resources/gui/looknfeel" "FileSystem" "General")
                       ("resources/gui/lua_scripts" "FileSystem" "General")
                       ("resources/gui/schemes" "FileSystem" "General"))))
  (setf *scene-manager* (make-scene-manager "OctreeSceneManager"))
  (setf *root-node* (root-node))
  (setf *timer* (make-timer))

  ;; camera
  (setf *camera* (make-camera :position #(115.0 50.0 115.0)
                              :look-at #(-10.0 0.0 -10.0)
                              :near-clip-distance 1.0))

  ;; viewport
  (setf *viewport* (make-viewport *camera*
                                  :background-colour '(0.0 0.0 0.0 1.0)))

  ;; light
  (setf *light* (make-light :diffuse-colour #(0.9 0.9 0.9 1.0)
                            ;; I don't think this makes a difference.
                            ;:direction (vector-normalise #(-1.0 -0.5 -0.1))
                            :direction  #(-1.0 -0.5 -0.1)
                            :position #(0.0 10.0 0.0)
                            :specular-colour #(0.9 0.9 0.9 1.0)
                            :type :lt-directional))

  ;; misc
  (set-ambient-light *scene-manager* #(0.2 0.2 0.2 1.0))
  (set-shadow-technique *scene-manager* :shadowtype-none)

  ;; water surface
  (let ((mo (create-water-surface 0.0 0.0 0.0 200.0 :material "Test/Blue80"
                                  :grid-size *water-grid-size*)))
    (setf *water-mo* mo)
    (setf *water-node* (make-child-scene-node))
    (attach-object *water-node* (pointer-to mo)))

  ;; sphere
  (let ((entity (make-entity :name "sphere" :prefab-type :pt-sphere)))
    (set-material-name entity "Test/Red")
    (setf *sphere* (make-child-scene-node))
    (attach-object *sphere* (pointer-to entity)))
  (set-scale *sphere* #(0.4 0.4 0.4))
  (set-position *sphere* #(0.0 25.0 0.0))

  ;; display
  (new-frame)

  ;;; CEGUI

  (setf okra-bindings::*cegui-actions* *cegui-actions*)

  (setf *cegui-renderer*
        (okra-cegui::create-renderer (pointer-to *render-window*)
                                     (pointer-to *scene-manager*)))
  (setf *cegui-system* (okra-cegui::create-system *cegui-renderer*))

  (okra-cegui::load-scheme "AquaLookSkin.scheme")
  (okra-cegui::set-default-mouse-cursor "AquaLook" "MouseArrow")
  (okra-cegui::mouse-cursor-set-image (okra-cegui::get-default-mouse-cursor))
  (okra-cegui::set-default-font "BlueHighway-12")

  (okra-cegui::inject-mouse-position (/ (get-actual-width *viewport*) 2.0)
                                     (/ (get-actual-height *viewport*) 2.0))

  ;; XXX: CEGUI is giving me problems on Linux, someone figure this out please.
  (handler-case
      (progn
        (setf *cegui-sheet* (okra-cegui::load-window-layout "flock.layout"))
        (okra-cegui::set-gui-sheet *cegui-sheet*)
        ;; see: http://www.cegui.org.uk/wiki/index.php/EventLookup
        (okra-cegui::subscribe-event (okra-cegui::get-window "quitButton")
                                     "Clicked")
        (okra-cegui::subscribe-event (okra-cegui::get-window "flockSettings")
                                     "CloseClicked"))
    (t (e) (format t "[flock] CEGUI layout not loaded: ~S~%" e)))

  ;;; clois-lane

  (let ((wh (get-window-handler (pointer-to *render-window*))))
    (setf *input-system* (clois-lane:create-input-system (mkstr wh)
                                                         :hide-mouse t)))
  (clois-lane:set-actions *actions*)
  (setf *running* t))


;;; Main Loop

(defun main-loop ()
  (clois-lane:set-actions *actions*)
  (setf *running* t)
  (loop with previous-time = (get-microseconds *timer*)
        with fps-time = 0
        with time = 0
        while *running*
        for current-time = (get-microseconds *timer*)
        for delta-time = (/ (- current-time previous-time) 1000000.0)
        for fps = (/ 1.0 delta-time)
        initially (format t "fps: ~5,2F" fps)
        do (setf previous-time current-time)
           (incf time delta-time)
           (clois-lane:set-window-extents (get-actual-width *viewport*)
                                          (get-actual-height *viewport*))
           (clois-lane:capture)
           (do-movement)
           ;; XXX: This is until the Linux CEGUI has been resolved.
           (when (> (- time fps-time) 1.0)
             (loop repeat 10 do (princ #\Backspace))
             (format t "fps: ~5,2F" fps)
             (force-output)
             (setf fps-time time))
           (update-water-surface *water-mo* 0.0 0.0 0.0 200.0
                                 :grid-size *water-grid-size*)
           (new-frame)))


;;; for development so i have to type less

(initialise-application)
(main-loop)
(quit)
