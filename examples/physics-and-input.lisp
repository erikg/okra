;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; okra-buclet-and-clois.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; This is now partly implemented (up to "Free the physics"):
;;;; o http://gafferongames.com/game-physics/fix-your-timestep/

;; for development
;#+sbcl (setf *muffled-warnings* 'implicit-generic-function-warning)


;;; Packages

(asdf :buclet)
(asdf :clois-lane)
(asdf :okra)

(in-package :okra)


;;; Variables

;; for clois-lane
(defvar *actions*
  '(;(:key-default . echo-self)
    ;(:mouse-button-default . echo-self)
    ;(:mouse-move-default . echo-self)
    (:kc-escape . stop-running)   (:kc-w . toggle-wireframe)
    (:kc-space  . shoot-cube)     (:kc-a . add-cubes)
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

;; for Buclet
(defparameter *dynamics-world* nil)
(defparameter *fall-shape* nil)
(defparameter *ground-rigid-body* nil)
(defparameter *ground-shape* nil)
(defparameter *physics-sdk* nil)

;; for Okra
(defparameter *camera* nil)
(defparameter *debug-window* nil)
(defparameter *light* nil)
(defparameter *overlay* nil)
(defparameter *overlay-manager* nil)
(defparameter *timer* nil)
(defparameter *viewport* nil)

;; for Buclet & Okra
(defparameter *cubes* nil)
(defparameter *triangle-count* 0)


;;; Functions

(defun do-movement ())  ; dummy definition to suppress warnings


(defun create-cube (width &optional (material "BaseWhiteNoLighting"))
  (let* ((mo (make-manual-object))
         (halfwit (/ width 2.0))
         (x+ halfwit) (x- (- 0.0 halfwit))
         (y+ halfwit) (y- (- 0.0 halfwit))
         (z+ halfwit) (z- (- 0.0 halfwit)))
    (begin mo material :ot-triangle-list)

    ;; 0 1 5
    (position mo (vector x+ y- z+))  ; 0
    (normal mo (vector 1.0 0.0 0.0))
    (position mo (vector x+ y- z-))  ; 1
    (normal mo (vector 1.0 0.0 0.0))
    (position mo (vector x+ y+ z-))  ; 5
    (normal mo (vector 1.0 0.0 0.0))

    ;; 0 5 4
    (position mo (vector x+ y- z+))  ; 0
    (normal mo (vector 1.0 0.0 0.0))
    (position mo (vector x+ y+ z-))  ; 5
    (normal mo (vector 1.0 0.0 0.0))
    (position mo (vector x+ y+ z+))  ; 4
    (normal mo (vector 1.0 0.0 0.0))

    ;; 2 7 3
    (position mo (vector x- y- z+))  ; 2
    (normal mo (vector -1.0 0.0 0.0))
    (position mo (vector x- y+ z-))  ; 7
    (normal mo (vector -1.0 0.0 0.0))
    (position mo (vector x- y- z-))  ; 3
    (normal mo (vector -1.0 0.0 0.0))

    ;; 2 6 7
    (position mo (vector x- y- z+))  ; 2
    (normal mo (vector -1.0 0.0 0.0))
    (position mo (vector x- y+ z+))  ; 6
    (normal mo (vector -1.0 0.0 0.0))
    (position mo (vector x- y+ z-))  ; 7
    (normal mo (vector -1.0 0.0 0.0))

    ;; 0 6 2
    (position mo (vector x+ y- z+))  ; 0
    (normal mo (vector 0.0 0.0 1.0))
    (position mo (vector x- y+ z+))  ; 6
    (normal mo (vector 0.0 0.0 1.0))
    (position mo (vector x- y- z+))  ; 2
    (normal mo (vector 0.0 0.0 1.0))

    ;; 0 4 6
    (position mo (vector x+ y- z+))  ; 0
    (normal mo (vector 0.0 0.0 1.0))
    (position mo (vector x+ y+ z+))  ; 4
    (normal mo (vector 0.0 0.0 1.0))
    (position mo (vector x- y+ z+))  ; 6
    (normal mo (vector 0.0 0.0 1.0))

    ;; 1 7 5
    (position mo (vector x+ y- z-))  ; 1
    (normal mo (vector 0.0 0.0 -1.0))
    (position mo (vector x- y+ z-))  ; 7
    (normal mo (vector 0.0 0.0 -1.0))
    (position mo (vector x+ y+ z-))  ; 5
    (normal mo (vector 0.0 0.0 -1.0))

    ;; 1 3 7
    (position mo (vector x+ y- z-))  ; 1
    (normal mo (vector 0.0 0.0 -1.0))
    (position mo (vector x- y- z-))  ; 3
    (normal mo (vector 0.0 0.0 -1.0))
    (position mo (vector x- y+ z-))  ; 7
    (normal mo (vector 0.0 0.0 -1.0))

    ;; 4 7 6
    (position mo (vector x+ y+ z+))  ; 4
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x- y+ z-))  ; 7
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x- y+ z+))  ; 6
    (normal mo (vector 0.0 1.0 0.0))

    ;; 4 5 7
    (position mo (vector x+ y+ z+))  ; 4
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x+ y+ z-))  ; 5
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x- y+ z-))  ; 7
    (normal mo (vector 0.0 1.0 0.0))

    ;; 0 2 3
    (position mo (vector x+ y- z+))  ; 0
    (normal mo (vector 0.0 -1.0 0.0))
    (position mo (vector x- y- z+))  ; 2
    (normal mo (vector 0.0 -1.0 0.0))
    (position mo (vector x- y- z-))  ; 3
    (normal mo (vector 0.0 -1.0 0.0))

    ;; 0 3 1
    (position mo (vector x+ y- z+))  ; 0
    (normal mo (vector 0.0 -1.0 0.0))
    (position mo (vector x- y- z-))  ; 3
    (normal mo (vector 0.0 -1.0 0.0))
    (position mo (vector x+ y- z-))  ; 1
    (normal mo (vector 0.0 -1.0 0.0))

    (loop for i from 0 to 35 do (index mo i))

    (end mo)
    (incf *triangle-count* 12)
    mo))


(defun create-square (x y z width &key (filled nil)
                      (material "BaseWhiteNoLighting"))
  (let* ((mo (make-manual-object))
         (halfwit (/ width 2.0))
         (x1 (+ x halfwit)) (x2 (- x halfwit))
         (z1 (+ z halfwit)) (z2 (- z halfwit)))
    (set-cast-shadows mo nil)
    (if filled
        (begin mo material :ot-triangle-strip)
        (begin mo material :ot-line-strip))
    (position mo (vector x1 y z1))
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x1 y z2))
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x2 y z2))
    (normal mo (vector 0.0 1.0 0.0))
    (position mo (vector x2 y z1))
    (normal mo (vector 0.0 1.0 0.0))
    (index mo 0)
    (index mo 1)
    (if filled
        (progn
          (index mo 3)
          (index mo 2))
        (progn
          (index mo 2)
          (index mo 3)
          (index mo 0)))
    (end mo)
    (incf *triangle-count* 2)
    mo))


;; ugly but suffices for now, i'm in it for the cubes
(defun create-checkerboard (x y z width &optional (checker-size 10.0))
  (let ((halfwit (/ width 2.0)))
    (loop for ix from (- x halfwit) below (+ x halfwit) by checker-size
          do (loop for iz from (- z halfwit ) below (+ z halfwit) by checker-size
                   for colour = (if (evenp (floor (/ (+ ix iz) 10.0)))
                                    "Test/Blue80"
                                    "Test/Blue90")
                   for square = (create-square ix y iz checker-size :filled t
                                               :material colour)
                   do (attach-object *root-node* (pointer-to square))))))


(defun debug-text (text)
  (set-caption *debug-window*
    (mkfstr "=== Okra, Buclet & clois-lane example ===~%"
            "       ESC : quits~%"
            " left or S : move left~%"
            "right or F : move right~%"
            "   up or E : move forward~%"
            " down or D : move backward~%"
            " PgUp or R : move up~%"
            " PgDn or V : move down~%"
            "     SPACE : shoot one cube straight ahead~%"
            "         A : adds more cubes~%"
            "         W : toggles wireframe mode~%"
            "~%"
            "If you keep the right mouse button pressed~%"
            "you can use the mouse to look around.~%"
            "~%"
            "-----------------------------------------~%"
            "~%"
            text)))


;; Using this for now since the incomplete "Free the physics" version has
;; a visually unpleasing stutter (as explained in Gaffer).
;;
;; Apparently Bullet already does a Gaffer internally so I can just pass the
;; time since the last step.
;;
;; And then again, apparently not since the C-API uses maxSubSteps=1 (default)
(defun start-simulation-X ()
  (clois-lane:set-actions *actions*)
  (setf *running* t)
  (loop with previous-time = (get-microseconds *timer*)
        with time = 0
        while *running*
        for current-time = (get-microseconds *timer*)
        for delta-time = (/ (- current-time previous-time) 1000000)
        for fps = (/ 1 delta-time)
        do (setf previous-time current-time)
           (incf time delta-time)
           (buclet:step-simulation *dynamics-world* (coerce time 'single-float))
           (loop for cube in *cubes*
                 for frb = (car cube)
                 for node = (cdr cube)
                 for o = (buclet:get-orientation frb)
                 for p = (buclet:get-position frb)
                 do (set-orientation node (vector (fourth o) (first o)
                                                  (second o) (third o)))
                    (set-position node (vector (first p) (second p) (third p))))
           ;; set mouse region since the window might have been resized
           (clois-lane:set-window-extents (get-actual-width *viewport*)
                                          (get-actual-height *viewport*))
           (clois-lane:capture)
           (do-movement)
           (debug-text (format nil "fps: ~,2F~%cubes: ~A~%triangles: ~A~%"
                               fps (length *cubes*) *triangle-count*))
           (new-frame)))

;(defun start-simulation-free-the-physics ()
(defun start-simulation ()
  (clois-lane:set-actions *actions*)
  (setf *running* t)
  (loop with accumulator = 0
        with fixed-time-step = (coerce (/ 1 60) 'single-float)
        with previous-time = (get-microseconds *timer*)
        with time = 0
        while *running*
        for current-time = (get-microseconds *timer*)
        for delta-time = (/ (- current-time previous-time) 1000000)
        for fps = (/ 1 delta-time)
        do (setf previous-time current-time)
           (incf accumulator delta-time)
           (loop while (>= accumulator fixed-time-step)
                 do ;; Process input at a fixed rate.
                    (clois-lane:set-window-extents
                      (get-actual-width *viewport*)
                      (get-actual-height *viewport*))
                    (clois-lane:capture)
                    (do-movement)
                    ;; Gaffer?: previousState = currentState
                    (buclet:step-simulation *dynamics-world* fixed-time-step)
                    (incf time fixed-time-step)
                    (decf accumulator fixed-time-step))
           ;; Gaffer?: const float alpha = accumulator / dt;
           ;; Gaffer?: state = currentState*alpha + previousState*(1.0f-alpha);
           (loop for cube in *cubes*
                 for frb = (car cube)
                 for node = (cdr cube)
                 for o = (buclet:get-orientation frb)
                 for p = (buclet:get-position frb)
                 do (set-orientation node (vector (fourth o) (first o)
                                                  (second o) (third o)))
                    (set-position node (vector (first p) (second p) (third p))))
           (debug-text (format nil "fps: ~,2F~%cubes: ~A~%triangles: ~A~%"
                               fps (length *cubes*) *triangle-count*))
           (new-frame)))


(defun setup-cubes ()
  (loop for x from 40.0 to 68.0 by 7.0
        do (loop for y from 40.0 to 64.0 by 8.0
                 for cube = (create-cube 5.0 "Example/RedCube")
                 for frb = (buclet:create-rigid-body 1.0 *fall-shape*)
                 for node = (make-child-scene-node)
                 for z = (+ (- 105.0 x) (random 2.5))
                 do (buclet:set-orientation frb (list 0.4 0.5 0.6 (random 1.0)))
                    ;; getting this from the physics world since the initial settings
                    ;; above get changed and I don't understand quaternions
                    (let ((o (buclet:get-orientation frb)))
                      (set-orientation node (vector (fourth o) (first o)
                                                    (second o) (third o))))
                    (buclet:set-position frb (list x y z))
                    (set-position node (vector x y z))
                    (buclet:add-rigid-body *dynamics-world* frb)
                    (attach-object node (pointer-to cube))
                    (push (cons frb node) *cubes*)))
  (new-frame))


(defun start-ogre ()
  ;; Buclet init
  (setf *physics-sdk* (buclet:new-bullet-sdk))
  (setf *dynamics-world* (buclet:create-dynamics-world *physics-sdk*))

  ;; Okra init
  (setf *render-window*
        (okra-window :render-system #-windows "OpenGL Rendering Subsystem"
                                    #+windows "Direct3D9 Rendering Subsystem"
                     :resources '(("resources" "FileSystem" "General"))))
  (setf *scene-manager* (make-scene-manager "OctreeSceneManager"))
  (setf *root-node* (root-node))

  ;; timer
  (setf *timer* (make-timer))

  ;; clois-lane init
  (let ((wh (get-window-handler (pointer-to *render-window*))))
    (setf *input-system* (clois-lane:create-input-system (mkstr wh))))

  ;; Setting the shadow technique should be the first thing done in a scene.
  ;;
  ;; Cheapest and lowest quality shadow technique, only one with
  ;; :shadowtype-texture-modulative that works on my old graphics adapter.
  (set-shadow-technique *scene-manager* :shadowtype-none)
                                        ;:shadowtype-texture-additive)
                                        ;:shadowtype-texture-modulative)
                                        ;:shadowtype-stencil-additive)
                                        ;:shadowtype-stencil-modulative)

  ;; camera
  (setf *camera* (make-camera :look-at #(50.0 30.0 50.0)
                              :near-clip-distance 5.0
                              :position #(90.0 80.0 100.0)))

  ;; viewport
  (setf *viewport* (make-viewport *camera*))
  (set-aspect-ratio *camera* (* 1.0 (/ (get-actual-width *viewport*)
                                       (get-actual-height *viewport*))))

  ;; light
  (setf *light* (make-light :direction #(-0.408 -0.816 -0.408)
                            :position #(80.0 100.0 80.0)))
  (set-type *light* :lt-directional)
  (set-position *light* #(80.0 100.0 80.0))
  (set-diffuse-colour *light* #(1.0 1.0 1.0 1.0))
  (set-specular-colour *light* #(1.0 1.0 1.0 1.0))
  (set-direction *light* #(-0.408 -0.816 -0.408))

  ;; misc
  (set-ambient-light *scene-manager* #(0.2 0.2 0.2 1.0))

  ;; overlays
  (setf *overlay-manager* (make-overlay-manager))
  (setf *overlay* (make-instance 'overlay :pointer
                           (get-by-name *overlay-manager* "Okra/TestOverlay")))
  (show *overlay*)
  (setf *debug-window* (make-instance 'overlay-element :pointer
              (get-overlay-element *overlay-manager* "Okra/TestTextArea" nil)))

  ;; Okra 'ground'
  (create-checkerboard 50.0 0.0 50.0 200.0)

  ;; Buclet 'ground'
  (setf *ground-shape* (buclet:new-static-plane-shape '(0.0 1.0 0.0) 1.0))
  (setf *ground-rigid-body* (buclet:create-rigid-body 0.0 *ground-shape*))
  (buclet:set-position *ground-rigid-body* '(0.0 -1.0 0.0))
  (buclet:set-orientation *ground-rigid-body* '(0.0 0.0 0.0 1.0))
  (buclet:add-rigid-body *dynamics-world* *ground-rigid-body*)

  ;; Cubes
  (setf *fall-shape* (buclet:new-box-shape 2.5 2.5 2.5))
  (setup-cubes))


;;; Key Actions

(defun add-cubes (key char state)
  (declare (ignore key char))
  (when (equal state :released)
    (setup-cubes)))


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


(defun move-backward (key char state)
  (declare (ignore key char))
  (if (equal state :pressed)
      (setf *move-backward* t)
      (setf *move-backward* nil)))


(defun move-down (key char state)
  (declare (ignore key char))
  (if (equal state :pressed)
      (setf *move-down* t)
      (setf *move-down* nil)))


(defun move-forward (key char state)
  (declare (ignore key char))
  (if (equal state :pressed)
      (setf *move-forward* t)
      (setf *move-forward* nil)))


(defun move-left (key char state)
  (declare (ignore key char))
  (if (equal state :pressed)
      (setf *move-left* t)
      (setf *move-left* nil)))


(defun move-right (key char state)
  (declare (ignore key char))
  (if (equal state :pressed)
      (setf *move-right* t)
      (setf *move-right* nil)))


(defun move-up (key char state)
  (declare (ignore key char))
  (if (equal state :pressed)
      (setf *move-up* t)
      (setf *move-up* nil)))


(defun shoot-cube (key char state)
  (declare (ignore key char))
  (when (equal state :released)
    (let ((cube (create-cube 5.0 "Example/RedCube"))
          (frb (buclet:create-rigid-body 1.0 *fall-shape*))
          (node (make-child-scene-node)))
      (buclet:set-orientation frb (list 0.4 0.5 0.6 (random 1.0)))
      ;; getting this from the physics world since the initial settings
      ;; above get changed and I don't understand quaternions
      (let ((o (buclet:get-orientation frb)))
        (set-orientation node (vector (fourth o) (first o) (second o)
                                      (third o))))
      (let ((cd (get-direction *camera*))
            (cp (get-position *camera*)))
        (buclet:set-linear-velocity frb (vector (* (svref cd 0) 100.0)
                                                (* (svref cd 1) 100.0)
                                                (* (svref cd 2) 100.0)))
        (buclet:set-position frb cp)
        (set-position node cp))
      (buclet:add-rigid-body *dynamics-world* frb)
      (attach-object node (pointer-to cube))
      (push (cons frb node) *cubes*))))


(defun stop-running (key char state)
  (declare (ignore key char))
  (when (equal state :released)
    (setf *running* nil)))


(defun toggle-mouse-look (button state)
  (declare (ignore button))
  (if (equal state :pressed)
      (setf *mouse-look* t)
      (setf *mouse-look* nil)))


(let ((pm-mode :pm-solid))
  (defun toggle-wireframe (key char state)
    (declare (ignore key char))
    (when (equal state :released)
      (if (equal pm-mode :pm-solid)
          (progn (set-polygon-mode *camera* :pm-wireframe)
                 (setf pm-mode :pm-wireframe))
          (progn (set-polygon-mode *camera* :pm-solid)
                 (setf pm-mode :pm-solid))))))


;;; Main Program

(setf *random-state* (make-random-state t))

(start-ogre)
(start-simulation)
(quit)
