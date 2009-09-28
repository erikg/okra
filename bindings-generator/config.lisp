;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; config.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Variables

(defparameter *verbose* nil)

(defparameter *base-classes* (make-hash-table :test 'equal))
(defparameter *classes* (make-hash-table :test 'equal))
(defparameter *enum-types* (make-hash-table :test 'equal))
(defparameter *enums* (make-hash-table :test 'equal))
(defparameter *members* (make-hash-table :test 'equal))
(defparameter *typedefs* (make-hash-table :test 'equal))

;;; Bindings should be generated for the following files.  Maybe at some
;;; indefinite time in the future we'll be able to process all files.
;;; (I doubt it and I'm not even sure we'd want to do that.)
(defparameter *doxygen-files*
  '("xml/class_ogre_1_1_animation_state.xml"
    "xml/class_ogre_1_1_camera.xml"
    "xml/class_ogre_1_1_entity.xml"
    "xml/class_ogre_1_1_frustum.xml"
    "xml/class_ogre_1_1_light.xml"
    "xml/class_ogre_1_1_manual_object.xml"
    "xml/class_ogre_1_1_movable_object.xml"
    "xml/class_ogre_1_1_node.xml"
    "xml/class_ogre_1_1_overlay.xml"
    "xml/class_ogre_1_1_overlay_element.xml"
    "xml/class_ogre_1_1_overlay_manager.xml"
    "xml/class_ogre_1_1_ray_scene_query.xml"
    "xml/class_ogre_1_1_render_operation.xml"
    "xml/class_ogre_1_1_render_target.xml"
    "xml/class_ogre_1_1_render_window.xml"
    "xml/class_ogre_1_1_resource_group_manager.xml"
    "xml/class_ogre_1_1_root.xml"
    "xml/class_ogre_1_1_scene_manager.xml"
    "xml/class_ogre_1_1_scene_node.xml"
    "xml/class_ogre_1_1_timer.xml"
    "xml/class_ogre_1_1_viewport.xml"
    ;"xml/class_ogre_1_1_window_event_utilities.xml"
    ))

;; Hmpf..
(defparameter *doxygen-enums-files*
              (append1 *doxygen-files* "xml/namespace_ogre.xml"))
(defparameter *doxygen-typedefs-files*
              (append1 *doxygen-files* "xml/namespace_ogre.xml"))

;; regular expressions
(defparameter *regex-con* (cl-ppcre:create-scanner "^\\s*(.*?)\\s*$"))
(defparameter *regex-dec* (cl-ppcre:create-scanner "^\\s*(-?\\d+?)\\s*$"))
(defparameter *regex-hex* (cl-ppcre:create-scanner "^\\s*0x(\\S+?)\\s*$"))

(defparameter *regex-caps* (cl-ppcre:create-scanner "[A-Z]"))
(defparameter *regex-crap* (cl-ppcre:create-scanner "\\("))
(defparameter *regex-stl* (cl-ppcre:create-scanner "\\S+< .* >"))

;; used by the HTML-TEMPLATE package
(setf *default-template-pathname* (merge-pathnames "templates/"))
(setf *string-modifier* #'cl:identity)
(setf *template-start-marker* "[")
(setf *template-end-marker* "]")

;; templates
(defparameter *templates* (make-hash-table))
(defparameter *template-list*
  '((:cl-asd-template . #p"cl-asd.t")
    (:cl-enums-template . #p"cl-enums.t")
    (:cl-file-template . #p"cl-file.t")
    (:cl-generics-template . #p"cl-generics.t")
    (:cl-package-template . #p"cl-package.t")
    (:cl-typedefs-template . #p"cl-typedefs.t")
    (:cpp-cmakelists-template . #p"cpp-cmakelists.t")
    (:cpp-file-template . #p"cpp-file.t")))

;;; *blacklist* is basically a dump for members that I don't want to or don't
;;; know how to deal with.
(defparameter *blacklist*
  '((:name "addListener" :type "void" :args (("Listener*" . "s")))
    (:name "copyContentsToMemory" :type "void" :args (("const PixelBox&" . "dst") ("FrameBuffer" . "buffer")))
    ;; I can't differentiate between pointers yet.
    (:name "enableCustomNearClipPlane" :type "void" :args (("const Plane&" . "plane")))
    ;; arbitrary choice
    (:name "enableReflection" :type "void" :args (("const Plane&" . "p")))
    (:name "forwardIntersect" :type "void" :args (("const Plane&" . "worldPlane") ("std::vector< Vector4 >*" . "intersect3d")))
    ;; arbitrary choice
    (:name "getAttachedObjectIterator" :type "ConstObjectIterator" :args "void")
    ;; returns an uint16* (UTF char)
    (:name "getCaption" :type "const DisplayString&" :args "void")
    ;; arbitrary choice
    (:name "getChildIterator" :type "ConstChildNodeIterator" :args "void")
    (:name "getListener" :type "Listener*" :args "void")
    ;; arbitrary choice for RenderTarget version
    (:name "getMetrics" :type "void" :args (("unsigned int&" . "width") ("unsigned int&" . "height") ("unsigned int&" . "colourDepth") ("int&" . "left") ("int&" . "top")))
    (:name "getQueuedRenderableVisitor" :type "SceneMgrQueuedRenderableVisitor*" :args "void")
    (:name "getSpecialCaseRenderQueueMode" :type "SpecialCaseRenderQueueMode" :args "void")
    (:name "getWindowPlanes" :type "const std::vector< Plane >&" :args "void")
    ;; no idea how to handle this one
    (:name "projectSphere" :type "bool" :args (("const Sphere&" . "sphere") ("Real*" . "left") ("Real*" . "top") ("Real*" . "right") ("Real*" . "bottom")))
    (:name "removeListener" :type "void" :args (("Listener*" . "s")))
    (:name "resourceExists" :type "bool" :args (("ResourceGroup*" . "group") ("const String&" . "filename")))
    (:name "resourceModifiedTime" :type "time_t" :args (("ResourceGroup*" . "group") ("const String&" . "filename")))
    (:name "setListener" :type "void" :args (("Listener*" . "listener")))
    (:name "setQueuedRenderableVisitor" :type "void" :args (("SceneMgrQueuedRenderableVisitor*" . "visitor")))))
