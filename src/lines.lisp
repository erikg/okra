;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; lines.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)

(in-package :okra)


(defun create-line (line &key (material "BaseWhiteNoLighting"))
  ;(format t "[line] ~S~%" line)
  (let ((from (first line))
        (to (second line))
        (mo (make-manual-object)))
    (begin mo material :ot-line-list)
    (position mo (vector (svref from 0) 0.1 (svref from 1)))
    (position mo (vector (svref to 0) 0.1 (svref to 1)))
    (end mo)
    mo))


;; From: http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
(defun line-line-intersection (line1 line2)
;;; Author: Paul Reiners
;;; Created: 2008-03-10 17:18:13
;;; From algorithm by Paul Bourke given here:
;;;     http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
  (flet ((get-intersection (x1 y1 x2 y2 x3 y3 x4 y4)
           (let ((denom (- (* (- y4 y3) (- x2 x1)) (* (- x4 x3) (- y2 y1))))
                 (ua-num (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3))))
                 (ub-num (- (* (- x2 x1) (- y1 y3)) (* (- y2 y1) (- x1 x3)))))
             (cond ;; If the denominator and numerator for the equations for
                   ;;ua and ub are 0 then the two lines are coincident.
                   ((and (zerop denom) (zerop ua-num) (zerop ub-num))
                    (list x1 y1))
                   ;; If the denominator for the equations for ua and ub is 0
                   ;; then the two lines are parallel.
                   ((zerop denom) nil)
                   (t (let ((ua (/ ua-num denom))
                            (ub (/ ub-num denom)))
                        (if (and (<= 0 ua 1) (<= 0 ub 1))
                            ;; x = x1 + ua (x2 - x1)
                            ;; y = y1 + ua (y2 - y1)
                            (list (+ x1 (* ua (- x2 x1)))
                                  (+ y1 (* ua (- y2 y1))))
                            nil)))))))
    (let* ((x1 (elt (first  line1) 0))  (y1 (elt (first  line1) 1))
           (x2 (elt (second line1) 0))  (y2 (elt (second line1) 1))
           (x3 (elt (first  line2) 0))  (y3 (elt (first  line2) 1))
           (x4 (elt (second line2) 0))  (y4 (elt (second line2) 1))
           (intersect (get-intersection x1 y1 x2 y2 x3 y3 x4 y4)))
      (if intersect
          (coerce intersect 'vector)
          nil))))


;; From: http://mathforum.org/library/drmath/view/54823.html
;(defun which-line-side (point line)
;  (let ((x (elt point 0))           (y (elt point 2))
;        (x1 (elt (first line) 0))   (y1 (elt (first line) 2))
;        (x2 (elt (second line) 0))  (y2 (elt (second line) 2)))
;    (- y y1 (* (/ (- y2 y1) (- x2 x1))
;               (- x x1)))))
(defun which-line-side (point line)
  "Returns 1 if POINT is on the right side of LINE, 0 if POINT is on LINE and
  -1 if POINT is on the left side of LINE."
  (let* ((v1 (first line))
         (v2 (second line))
         (vcp (vector-cross-product
                (vector-from-coords (elt v1 0) 0.0 (elt v1 2)
                                    (elt v2 0) 0.0 (elt v2 2))
                (vector-from-coords (elt v1 0) 0.0 (elt v1 2)
                                    (elt point 0) 0.0 (elt point 2))))
         (n (elt vcp 1)))
    (cond ((< n 0) -1)
          ((= n 0) 0)
          ((> n 0) 1))))


(defun random-line (bounding-box)
  (let* ((x-min (elt (first bounding-box) 0))
         (y-min (elt (first bounding-box) 1))
         (x-max (elt (second bounding-box) 0))
         (y-max (elt (second bounding-box) 1)))
    (labels ((get-side-coords (side)
               (case side
                 (0 (vector (+ x-min (random (- x-max x-min))) y-min))
                 (1 (vector x-max (+ y-min (random (- y-max y-min)))))
                 (2 (vector (+ x-min (random (- x-max x-min))) y-max))
                 (3 (vector x-min (+ y-min (random (- y-max y-min))))))))
      (let* ((random-side (random 4))
             (side-a (get-side-coords random-side))
             (side-b (get-side-coords (elt (remove random-side '(0 1 2 3))
                                           (random 3)))))
        (list side-a side-b)))))


(defun test-lli (key char state)
  (declare (ignore key char))
  (when (equal state :released)
    (destroy-all-entities *scene-manager*)
    (destroy-all-manual-objects *scene-manager*)
    (terpri)
    (let* ((line1 (random-line (polygon-bounding-box (first *polygons*))))
           (line2 (random-line (polygon-bounding-box (first *polygons*))))
           (intersect (line-line-intersection line1 line2)))
      (attach-object-okra (create-line line1))
      (attach-object-okra (create-line line2))
      (when intersect
        (format t "Lines intersect at: ~S~%" intersect)
        (let ((entity (make-entity :prefab-type :pt-sphere))
              (sphere (make-child-scene-node)))
          (set-material-name entity "Test/Red")
          (attach-object sphere (pointer-to entity))
          (set-scale sphere #(0.02 0.02 0.02))
          (set-position sphere (vector (elt intersect 0) 0.1
                                       (elt intersect 1))))))))
