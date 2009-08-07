;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; primitives.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)

(in-package :okra)


(defun create-box (width &optional (material "BaseWhiteNoLighting"))
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


(defun draw-debug-sphere (x y z)
  (let ((entity (make-entity :prefab-type :pt-sphere))
        (sphere (make-child-scene-node)))
    (set-material-name entity "Test/Red")
    (attach-object sphere (pointer-to entity))
    (set-scale sphere #(0.02 0.02 0.02))
    (set-position sphere (vector x y z))))
