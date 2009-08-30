;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; flock.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; note-to-self:
;;;;     - http://www.ogre3d.org/wiki/index.php/CodeSnippets#Shaders
;;;;     - http://artis.imag.fr/~Xavier.Decoret/resources/ogre/tutorial4.html

;;; Packages

(asdf :okra-cegui)
(asdf :clois-lane-cegui)  ; after okra-cegui to surpress warnings

(in-package :okra)


;;; Classes

(defclass bird ()
  ((direction :accessor direction-of :initarg :direction)
   (fly-speed :accessor fly-speed-of :initform 10.0)
   (manual-object :accessor manual-object-of :initarg :manual-object)
   (node :accessor node-of :initarg :node)
   (orientation :accessor orientation-of :initarg :orientation)
   (position :accessor position-of :initarg :position)
   (turn-speed :accessor turn-speed-of :initform 0.1)))


(defclass scene ()
  ((cameras :accessor cameras-of :initarg :cameras)
   (light :accessor lights-of :initarg :lights)
   (manager :accessor manager-of :initarg :manager)
   (root :accessor root-of :initarg :root)))


(defclass flock-scene (scene)
  ((birds :accessor birds-of :initarg :birds)
   (water :accessor water-of :initarg :water)))


(defclass water ()
  ((grid :accessor grid-of :initarg :grid :initform 10.0)
   (manual-object :reader manual-object-of :initarg :manual-object
                  :initform (error "must supply :manual-object"))
   (material :reader material-of :initarg material-of
             :initform "BaseWhiteNoLighting")
   (node :reader node-of :initarg :node :initform (error "must supply :node"))
   (position :accessor position-of :initarg :position :initform #(0.0 0.0 0.0))
   (size :accessor size-of :initarg :size :initform 100.0)
   (speed :accessor speed-of :initarg :speed :initform 0.025)))


;;; Parameters

(defparameter *tex-inc* 0.0)

(defparameter *water-grid-size* 10.0)
(defparameter *water-position* 0.0)
(defparameter *water-size* 300.0)
(defparameter *water-speed* 0.025)

(defparameter *birds* nil)
(defparameter *camera* nil)
(defparameter *cube* nil)
(defparameter *light* nil)
(defparameter *plane* nil)
(defparameter *sphere* nil)
(defparameter *timer* nil)
(defparameter *viewport* nil)
(defparameter *water-mo* nil)  ; manual object
(defparameter *water-node* nil)
(defparameter *triangle-count* 0)

;; for CEGUI
(defvar *cegui-actions*
  '((("quitButton" . "Clicked") . cegui-stop-running)
    (:default . echo-self)))

(defparameter *cegui-loaded* nil)  ; tmp: for the Linux CEGUI problems
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


;;; Bird Functions

(defun create-bird-modelX ()
  (let* ((mo (make-manual-object)))
    (begin mo "Bird" :ot-triangle-list)
    ;; top
    (position mo 1.0 0.0 0.0)
    (colour mo 0.0 0.0 0.0 1.0)
    (normal mo 0.0 1.0 0.0)
    (position mo -2.0 0.0 -1.0)
    (colour mo 1.0 1.0 1.0 1.0)
    (normal mo 0.0 1.0 0.0)
    (position mo -2.0 0.0 1.0)
    (colour mo 1.0 1.0 1.0 1.0)
    (normal mo 0.0 1.0 0.0)
    ;; bottom
    (position mo 1.0 0.0 0.0)
    (colour mo 1.0 0.0 0.0 1.0)
    (normal mo 0.0 -1.0 0.0)
    (position mo -2.0 0.0 1.0)
    (colour mo 0.0 0.0 1.0 1.0)
    (normal mo 0.0 -1.0 0.0)
    (position mo -2.0 0.0 -1.0)
    (colour mo 0.0 0.0 1.0 1.0)
    (normal mo 0.0 -1.0 0.0)
    (end mo)
    mo))

(defun create-bird-model ()
  (let ((mo (make-manual-object))
        (black 0.0)
        (white 1.0))
    (begin mo "Bird" :ot-triangle-list)
    ;; bottom
    (let* ((vn (vnormal (vsub #(-2.0 -0.5  1.0) #(1.0 0.0 0.0))
                        (vsub #(-2.0 -0.5 -1.0) #(1.0 0.0 0.0))))
           (vnx (elt vn 0))
           (vny (elt vn 1))
           (vnz (elt vn 2)))
      (position mo 1.0 0.0 0.0)
      (colour mo black black black 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 -0.5 1.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 -0.5 -1.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz))
    ;; top right
    (let* ((vn (vnormal (vsub #(-2.0  0.5 0.0) #(1.0 0.0 0.0))
                        (vsub #(-2.0 -0.5 1.0) #(1.0 0.0 0.0))))
           (vnx (elt vn 0))
           (vny (elt vn 1))
           (vnz (elt vn 2)))
      (position mo 1.0 0.0 0.0)
      (colour mo black black black 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 0.5 0.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 -0.5 1.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz))
    ;; top left
    (let* ((vn (vnormal (vsub #(-2.0 -0.5 -1.0) #(1.0 0.0 0.0))
                        (vsub #(-2.0  0.5 0.0) #(1.0 0.0 0.0))))
           (vnx (elt vn 0))
           (vny (elt vn 1))
           (vnz (elt vn 2)))
      (position mo 1.0 0.0 0.0)
      (colour mo black black black 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 -0.5 -1.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 0.5 0.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz))
    ;; back
    (let* ((vn (vnormal (vsub #(-2.0 -0.5 -1.0) #(-2.0 0.5 0.0))
                        (vsub #(-2.0 -0.5  1.0) #(-2.0 0.5 0.0))))
           (vnx (elt vn 0))
           (vny (elt vn 1))
           (vnz (elt vn 2)))
      (position mo -2.0 0.5 0.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 -0.5 -1.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz)
      (position mo -2.0 -0.5 1.0)
      (colour mo white white white 1.0)
      (normal mo vnx vny vnz))
    (end mo)
    mo))


(defun flock-birds (&optional (birds *birds*))
  (loop for bird in birds
        for i from 0
        for centre = #(0.0 20.0 0.0)
        for dir = (elt bird 2)
        for node = (elt bird 0)
        for pos = (elt bird 1)
        for distance-from-centre = (vlength (vsub pos centre))
        for vp+d = (vector (+ (elt pos 0) (elt dir 0))
                           (+ (elt pos 1) (elt dir 1))
                           (+ (elt pos 2) (elt dir 2)))
        for neighbours = (loop for other in birds
                               for opos = (elt other 1)
                               for dist = (vlength (vsub opos pos))
                               unless (or (equal other bird)
                                          (> dist 15.0))
                                 collect other)
        unless neighbours do
           (setf neighbours (list (elt birds (random (- (length birds) 1)))))
        do (if (> distance-from-centre 100.0)  ; keep them on the screen
               (progn
                 ;(setf (elt bird 2) (vnormalise (vsub pos centre)))
                 ;(setf (elt bird 1) (vadd pos (elt bird 2))))
                 (setf (elt bird 2) #(1.0 0.0 0.0))
                 (setf (elt bird 1) #(0.0 2.0 0.0)))
               (progn
                 (setf (elt bird 1) vp+d)
                 (setf (elt bird 2)
                       (loop with vd = (elt (first neighbours) 1)
                             for n from 1
                             for neighbour in (rest neighbours)
                             do (setf vd (vadd vd (elt neighbour 1)))
                             finally (return
                                       (vnormalise
                                         (vadd dir (vscale (vscale vd (/ 1 n))
                                                           0.1))))))))))


(defun make-bird (&key direction position (orientation #(1.0 0.0 0.0 0.0)))
  (let ((mo (create-bird-model))
        (node (make-child-scene-node)))
    (attach-object node (pointer-to mo))
    (set-position node (elt position 0) (elt position 1) (elt position 2))
    (make-instance 'bird :direction direction :manual-object mo :node node
                   :orientation orientation :position position)))


;(defun update-bird-positions-in-world (&optional (birds *birds*))
;  (loop for bird in birds
;        for node = (elt bird 0)
;        for pos = (elt bird 1)
;        do (set-position node (elt pos 0) (elt pos 1) (elt pos 2))))

(defun update-bird-positions-in-world (&optional (birds *birds*))
  (loop for bird in birds
        for node = (node-of bird)
        for pos = (position-of bird)
        do (set-position node (elt pos 0) (elt pos 1) (elt pos 2))))


;;; Water Functions

;(defun make-water (&key (grid 10.0) (material "Ocean/Calm")
;                   (position #(0.0 0.0 0.0)) (size 100.0) (speed 0.025))
;  (let ((mo (create-water-surface position size

(defun dy (x y z width)
  (* 20.0 (+ y (* (sin (+ (/ x 12.0) *water-position*))
                  (sin (+ (/ z (+ (/ width .75) (/ *water-position* 4.0)))
                          (perlin-noise (/ x width) 0.0 (/ z width))))))))


(defun water-surface-loop (manual-object x y z width grid-size)
  (loop with prev-dyxz = (make-array (+ 1 (ceiling (/ width grid-size)))
                                     :initial-element nil)
        with mo = manual-object
        with w/2 = (/ width 2.0)
        with w/x = (/ width 8.0)
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
                    (texture-coord mo (/ (- x x-min) w/x)
                                      (+ *tex-inc* (/ (- z z-min) w/x)))
                    (position mo x+ dyx+z+ z+)
                    (normal mo (elt fn1 0) (elt fn1 1) (elt fn1 2))
                    (texture-coord mo (/ (- x+ x-min) w/x)
                                      (+ *tex-inc* (/ (- z+ z-min) w/x)))
                    (position mo x+ dyx+z z)
                    (normal mo (elt fn1 0) (elt fn1 1) (elt fn1 2))
                    (texture-coord mo (/ (- x+ x-min) w/x)
                                      (+ *tex-inc* (/ (- z z-min) w/x)))
                    ;; 2nd grid triangle
                    (position mo x dyxz z)
                    (normal mo (elt fn2 0) (elt fn2 1) (elt fn2 2))
                    (texture-coord mo (/ (- x x-min) w/x)
                                      (+ *tex-inc* (/ (- z z-min) w/x)))
                    (position mo x dyxz+ z+)
                    (normal mo (elt fn2 0) (elt fn2 1) (elt fn2 2))
                    (texture-coord mo (/ (- x x-min) w/x)
                                      (+ *tex-inc* (/ (- z+ z-min) w/x)))
                    (position mo x+ dyx+z+ z+)
                    (normal mo (elt fn2 0) (elt fn2 1) (elt fn2 2))
                    (texture-coord mo (/ (- x+ x-min) w/x)
                                      (+ *tex-inc* (/ (- z+ z-min) w/x)))
                 finally (setf (aref prev-dyxz (+ i 1)) dyx+z+))))


(defun create-water-surface (x y z width &key (grid-size 4.0)
                             (material "BaseWhiteNoLighting"))
  (let* ((mo (make-manual-object)))
    (set-dynamic mo t)
    (begin mo material :ot-triangle-list)
    (water-surface-loop mo x y z width grid-size)
    (end mo)
    (incf *water-position* *water-speed*)
    (incf *triangle-count* (/ (* width width) grid-size))
    mo))


(defun update-water-surface (manual-object x y z width &key (grid-size 4.0))
  (begin-update manual-object 0)
  (water-surface-loop manual-object x y z width grid-size)
  (incf *tex-inc* 0.005)
  (end manual-object))


;;; Initialisation

(defun initialise-application ()
  (setf *render-window*
        (okra-window :width 1024 :height 768
                     :render-system #-windows "OpenGL Rendering Subsystem"
                                    #+windows "Direct3D9 Rendering Subsystem"
                     :resources '(("resources" "FileSystem" "General")
                                  ("resources/gui" "FileSystem" "General"))))
  (setf *scene-manager* (make-scene-manager "OctreeSceneManager"))
  (setf *root-node* (root-node))
  (setf *timer* (make-timer))

  (setf *camera* (make-camera :position #(150.0 30.0 150.0)
                              :look-at #(-10.0 10.0 -10.0)
                              :near-clip-distance 1.0))

  (setf *viewport* (make-viewport *camera*
                                  :background-colour '(0.0 0.0 0.0 1.0)))

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

  ;; bird model
  (loop repeat 50
        ;for dir = (vnormalise (vector (- (random 2.0) 1) (- (random 2.0) 1)
        ;                              (- (random 2.0) 1)))
        for dir = (vnormalise #(1.0 0.0 0.0))
        for pos = (vector (- (random 50.0) 25) (+ 10 (random 25.0))
                          (- (random 50.0) 25))
        for bird = (make-bird :direction dir :position pos)
        do (push bird *birds*))

  ;; water surface
  (let ((mo (create-water-surface 0.0 0.0 0.0 *water-size*
                                  :grid-size *water-grid-size*
                                  :material "Ocean/Calm")))
    (setf *water-mo* mo)
    (setf *water-node* (make-child-scene-node))
    (attach-object *water-node* (pointer-to mo)))

  ;(make-water :grid 10.0 :material "Ocean/Calm" :position #(0.0 0.0 0.0)
  ;            :size 300.0)

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
  ;(okra-cegui::set-default-font "BlueHighway-12")
  (okra-cegui::mouse-cursor-set-image (okra-cegui::get-default-mouse-cursor))

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
                                     "CloseClicked")
        (setf *cegui-loaded* t))
    (t (e) (format t "[flock] CEGUI layout not loaded: ~S~%" e)))

  ;;; clois-lane

  (let ((wh (get-window-handler (pointer-to *render-window*))))
    (setf *input-system* (clois-lane:create-input-system (mkstr wh)
                                                         :hide-mouse t)))
  (clois-lane:set-actions *actions*)
  (setf *running* t))


;;; Main Loop

;; see: http://www.titanium.hr/Irrlicht/Tutorials/GameLoop/Tutorial.htm
(defun main-loop ()
  (clois-lane:set-actions *actions*)
  (setf *running* t)
  (loop with fps-time = 0
        with step = 0.02
        with then = (get-microseconds *timer*)
        while *running*
        for now = (get-microseconds *timer*)
        for delta = (/ (- now then) 1000000.0)
        for accumulator = delta
        initially (unless *cegui-loaded*  ; XXX: Linux CEGUI probs
                    (format t "fps:  0.00"))
        do (setf then now)
           (when (> accumulator 0.25)
             (setf accumulator 0.25))
           (loop while (>= accumulator step)
                 do (incf *water-position* *water-speed*)
                    ;(flock-birds)
                    (decf accumulator step))
           (when (> accumulator 0)
             (incf *water-position* (* *water-speed* (/ accumulator step))))
           ;; XXX: This is until the Linux CEGUI has been resolved.
           (if *cegui-loaded*
               (progn
                 ;; I haven't check set-text in yet from my other machine :-|
                 (okra-cegui::set-text (okra-cegui::get-window "FPS")
                                       (format nil "~,2F" (/ 1.0 delta)))
                 (okra-cegui::set-text (okra-cegui::get-window "Triangles")
                                       (format nil "~D" *triangle-count*)))
               (when (> (- (/ now 1000000.0) fps-time) 1.0)
                 (loop repeat 10 do (princ #\Backspace))
                 (format t "fps: ~5,2F" (/ 1.0 delta))
                 (force-output)
                 (setf fps-time (/ now 1000000.0))))
           (update-bird-positions-in-world)
           (update-water-surface *water-mo* 0.0 0.0 0.0 *water-size*
                                 :grid-size *water-grid-size*)
           (clois-lane:set-window-extents (get-actual-width *viewport*)
                                          (get-actual-height *viewport*))
           (clois-lane:capture)
           (do-movement)
           (new-frame)))


;;; for development so i have to type less

(initialise-application)
(main-loop)
(quit)