;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; polygons.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)

(in-package :okra)


(defun create-polygon (vertices &key (material "BaseWhiteNoLighting"))
  (when (< (length vertices) 3)
    (error "[create-polygon] Need at least three vertices for a polygon!"))
  (let* ((mo (make-manual-object))
         (size (length vertices))
         (center (loop with x = 0.0 and y = 0.0 and z = 0.0
                       for vertex in vertices
                       do (incf x (svref vertex 0))
                          (incf y (svref vertex 1))
                          (incf z (svref vertex 2))
                       finally (return (vector (/ x size) (/ y size)
                                               (/ z size))))))
    (set-cast-shadows mo nil)
    (begin mo material :ot-triangle-list)
    ;; draw the rest
    (loop with last-vertex = (first vertices)
          for vertex in (append1 (rest vertices) (first vertices))
          for vcenter = (vector (svref center 0) (svref center 1)
                                (svref center 2))
          for vlast = (vector (svref last-vertex 0) (svref last-vertex 1)
                              (svref last-vertex 2))
          for vcurr = (vector (svref vertex 0) (svref vertex 1)
                              (svref vertex 2))
          for vnormal = (vector-normal (vector-substract vlast vcenter)
                                       (vector-substract vcurr vcenter))
          do (position mo vcenter)
             (normal mo vnormal)
             (position mo vlast)
             (normal mo vnormal)
             (position mo vcurr)
             (normal mo vnormal)
             (setf last-vertex vertex))
    ;; make the indexes
    (loop for i from 0 to (* size 3)
           do (index mo i))
    (end mo)
    (incf *triangle-count* (length vertices))
    mo))


(defun draw-polygon-outline (polygon)
  (loop with v1 = (first polygon)
        with v-from = (first polygon)
        for v-to in (rest polygon)
        do (attach-object-okra
             (create-line (list (vector (elt v-from 0) (elt v-from 2))
                                (vector (elt v-to 0) (elt v-to 2)))))
           (setf v-from v-to)
        finally (attach-object-okra
                  (create-line (list (vector (elt v-from 0) (elt v-from 2))
                                     (vector (elt v1 0) (elt v1 2)))))))


;; From: http://local.wasp.uwa.edu.au/~pbourke/geometry/insidepoly/
(defun point-inside-polygon-p (point polygon)
  (let ((intersections 0)
        (max-x (loop for vertex in polygon maximize (elt vertex 0)))
        ;(min-x (loop for vertex in polygon minimize (elt vertex 0)))
        (lines (append1 polygon (first polygon)))
        (y (elt point 2)))
    (loop with hline = (list (vector (elt point 0) (elt point 2))
                             (vector max-x y))
          with prev-vertex = (first lines)
          for vertex in (rest lines)
          when (line-line-intersection hline
                         (list (vector (elt prev-vertex 0) (elt prev-vertex 2))
                               (vector (elt vertex 0) (elt vertex 2)))) do
            (incf intersections)
          do (attach-object-okra (create-line hline)))
    (oddp intersections)))


(defun polygon-substract (p1 p2)
  "Substracts P2 from P1 and returns the resulting set of polygons."
  (draw-polygon-outline p2)
  (let* ((lines nil)  ; lines which cut p1
         (lines1 (copy-seq (append1 p1 (first p1))))   ; copy-seq necessary?
         (lines2 (copy-seq (append1 p2 (first p2)))))  ; idem
    ;; add the intersection points as a vertex
    (loop with from-v2 = (first lines2)
          for to-v2 in (rest lines2)
          for v2i from 0
          for l2 = (list (vector (elt from-v2 0) (elt from-v2 2))
                         (vector (elt to-v2 0) (elt to-v2 2)))
          do (setf lines1
                   (loop with new-lines1 = nil
                         with from-v1 = (first lines1)
                         for to-v1 in (rest lines1)
                         for l1 = (list (if (consp from-v1)
                                            (vector (elt (car from-v1) 0)
                                                    (elt (car from-v1) 2))
                                            (vector (elt from-v1 0)
                                                    (elt from-v1 2)))
                                        (if (consp to-v1)
                                            (vector (elt (car to-v1) 0)
                                                    (elt (car to-v1) 2))
                                            (vector (elt to-v1 0)
                                                    (elt to-v1 2))))
                         for isect = (line-line-intersection l2 l1)
                         do (push from-v1 new-lines1)
                            (when isect
                              (pushnew (cons l2 v2i) lines :test 'equalp)
                              (push (cons (vector (elt isect 0) 0.0
                                                  (elt isect 1))
                                          v2i)
                                    new-lines1)
                              (draw-debug-sphere (elt isect 0) 0.1
                                                 (elt isect 1)))
                            (setf from-v1 to-v1)
                         finally (push from-v1 new-lines1)
                                 (return (reverse new-lines1))))
             (setf from-v2 to-v2))
    ;; mark the original vertices if they're on the 'good' side of a line
    (loop for cons in (reverse lines)
          for line = (list (vector (elt (first (car cons)) 0) 0.0
                                   (elt (first (car cons)) 1))
                           (vector (elt (second (car cons)) 0) 0.0
                                   (elt (second (car cons)) 1)))
          for index = (cdr cons)
          do (setf lines1
                   (loop with new-lines = nil
                         for vertex in lines1
                         do (if (and (not (consp vertex))
                                     (= -1 (which-line-side vertex line)))
                                (push (cons vertex index) new-lines)
                                (push vertex new-lines))
                         finally (return (reverse new-lines)))))
    ;; split the list of marked vertices into a list of polygons
    (loop with hash = (make-hash-table)
          for cons? in (subseq lines1 0 (- (length lines1) 1))
          for vertex = (when (consp cons?) (car cons?))
          for poly-nr = (when (consp cons?) (cdr cons?))
          when vertex do (push vertex (gethash poly-nr hash))
          when (not poly-nr) do (push cons? (gethash -1 hash))
          finally (return (loop for v being the hash-values in hash
                                when (> (length v) 2) collect (reverse v))))))


(defun polygon-bounding-box (polygon)
  (loop for vertex in polygon
        minimize (elt vertex 0) into x-min
        minimize (elt vertex 2) into y-min
        maximize (elt vertex 0) into x-max
        maximize (elt vertex 2) into y-max
        finally (return (list (vector x-min y-min) (vector x-max y-max)))))
