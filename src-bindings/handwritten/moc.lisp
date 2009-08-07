;;;; moc.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; Bindings to Minimal Ogre Collision (see /3rd-party/moc-*)
;;;;
;;;; Usage: (raycast-from-camera *collision-tools* *render-window* *camera*
;;;;                             (sequence mouse-abs-x mouse-abs-y))
;;;;
;;;;        returns either nil or a list: (okra-vector3 c++-pointer distance)
;;;;
;;;;        You need to check that pointer against any pointer-to slots of
;;;;        your classes or directly use the bindings on it like:
;;;;
;;;;        (okra-bindings::ogre-movable-object-get-name (second result))
;;;;
;;;; NOTE!: Does not work on manual-objects that have been dynamically created
;;;;        like I tend to do in my examples.  They have to be converted to a
;;;;        mesh first.

(in-package :okra-bindings)


;;; Foreign Functions & Methods

(defcfun "moc_initialise"
    :pointer
  (scene-manager :pointer))

(defun initialise-collision-tools (scene-manager)
  (moc-initialise (pointer-to scene-manager)))


(defcfun "moc_raycast_from_camera"
    :boolean
  (collision-tools :pointer)
  (render-window :pointer)
  (camera :pointer)
  (mouse-coords okra-array2)
  (result :pointer)
  (movable-object :pointer)
  (closest-distance :pointer)
  (query-mask :uint32))

(defun raycast-from-camera (collision-tools render-window camera mouse-coords
                            &optional (query-mask #xffffffff))
  (setf (elt mouse-coords 0) (coerce (elt mouse-coords 0) 'single-float))
  (setf (elt mouse-coords 1) (coerce (elt mouse-coords 1) 'single-float))
  (with-foreign-objects ((closest-distance :float)
                         (movable-object :pointer)
                         (result 'okra-real 3))
    (let ((hit (moc-raycast-from-camera collision-tools
                                        (pointer-to render-window)
                                        (pointer-to camera) mouse-coords result
                                        movable-object closest-distance
                                        query-mask)))
      (if hit
          (list (vector (mem-aref result 'okra-real 0)
                        (mem-aref result 'okra-real 1)
                        (mem-aref result 'okra-real 2))
                (mem-ref movable-object :pointer)
                (mem-ref closest-distance :float))
          nil))))
