;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; common-lisp-config.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Variables

;(defparameter *lisp-out-dir* "generated/src-bindings/")
(defparameter *lisp-out-dir* "../src-bindings/")

(defparameter *lisp-files* nil)  ; for ASDF
(defparameter *lisp-symbols* nil)  ; for package.lisp

;;; I've declared a lot of crap here as a :pointer of which I don't
;;; even know what it is.  (Generally camelCaps names that don't end
;;; with a '*' or 'Ptr'.)
(defparameter *lisp-type-mapping*
  '(;; standard types
    ("void" . ":void")
    ("bool" . ":boolean")
    ("double" . ":double")
    ("float" . ":float")
    ("int" . ":int")
    ("long" . ":long")
    ("size_t" . ":unsigned-int")
    ("std::string" . ":string")
    ("uchar" . ":unsigned-char")
    ("uint" . ":unsigned-int")
    ("uint8" . ":uint8")
    ("uint16" . ":uint16")
    ("uint32" . ":uint32")
    ("unsigned char" . ":unsigned-char")
    ("unsigned int" . ":unsigned-int")
    ("unsigned long" . ":unsigned-long")
    ("unsigned long long" . ":unsigned-long-long")
    ("unsigned short" . ":unsigned-short")
    ("time_t" . ":long")  ; at least on my machine :-)
    ;; Ogre types
    ("AxisAlignedBox" . cl-array6)  ; does this compute with cpp-config.lisp?
    ("AnimableValuePtr" . ":pointer")
    ("AnimationStateIterator" . ":pointer")
    ("ChildNodeIterator" . ":pointer")
    ("ChildObjectListIterator" . ":pointer")
    ("ConstChildNodeIterator" . ":pointer")
    ("ConstObjectIterator" . ":pointer")
    ("ConstShadowTextureConfigIterator" . ":pointer")
    ("DataStreamListPtr" . ":pointer")
    ("DataStreamPtr" . ":pointer")
    ("FileInfoListPtr" . ":pointer")
    ("Light::LightTypes" . "light-types")
    ("Matrix3" . ":pointer")
    ("MeshPtr" . ":pointer")
    ("SceneManagerEnumerator::MetaDataIterator" . ":pointer")
    ("ObjectIterator" . ":pointer")
    ("Overlay2DElementsIterator" . ":pointer")
    ("OverlayMapIterator" . ":pointer")
    ("PlaneBoundedVolume" . ":pointer")
    ("Ray" . cl-array6)
    ("Real" . "okra-real")
    ("RenderOperation::OperationType" . "operation-type")
    ("RenderSystemList*" . cl-render-system-list)
    ("SceneManagerEnumerator::SceneManagerIterator" . ":pointer")
    ("ShadowRenderableListIterator" . ":pointer")
    ("String" . ":string")
    ("StringVector" . ":pointer")
    ("StringVectorPtr" . ":pointer")
    ("TemplateIterator" . ":pointer")
    ("TransformSpace" . ":pointer")
    ("UTFString" . ":string")
    ("Vector3" . cl-array3)
    ("Vector4" . cl-array4)
    ("ViewPoint" . ":pointer")
    ("const String&" . ":string")
    ("const AxisAlignedBox&" . cl-array6)
    ("const ColourValue&" . cl-array4)
    ("const DisplayString&" . ":string")
    ("const Matrix4&" . cl-array16)
    ("const Quaternion&" . cl-array4)
    ("const Plane&" . cl-plane)
    ("const Radian&" . "okra-real")
    ("const Sphere&" . cl-array4)
    ("const String&" . ":string")
    ("const Vector3&" . cl-array3)
    ("const Vector3*" . cl-array3)))
