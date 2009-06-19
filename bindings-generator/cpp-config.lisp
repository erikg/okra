;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; cpp-config.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Variables

;(defparameter *cpp-build-dir* "generated/libokra/build/")
;(defparameter *cpp-src-dir* "generated/libokra/src/")
(defparameter *cpp-build-dir* "../libokra/build/")
(defparameter *cpp-src-dir* "../libokra/src/")

(defparameter *cpp-files* nil)  ; for CMakeLists.txt

;; XXX: why do I sometimes see ":args nil"?
(defparameter *c-type-mapping*
  '(;; standard types (if not in *typedefs*)
    ("void" . "void")
    ("bool" . "bool")
    ("float" . "float")
    ("int" . "int")
    ("long" . "long")
    ("size_t" . "size_t")
    ;("uchar" . "uchar")
    ;("uint" . "unsigned int")
    ;("uint8" . "uint8")
    ;("uint32" . "uint32")
    ("unsigned int" . "unsigned int")
    ("unsigned long" . "unsigned long")
    ("unsigned short" . "unsigned short")
    ("time_t" . "time_t")
    ;; Ogre types
    ("AxisAlignedBox" . "AxisAlignedBox")  ; no clue what I'm doing here
    ("AnimableValuePtr" . "AnimableValuePtr")
    ("AnimationStateIterator" . "AnimationStateIterator")
    ("ChildNodeIterator" . "Node::ChildNodeIterator")
    ("ChildObjectListIterator" . "Entity::ChildObjectListIterator")
    ("ConstChildNodeIterator" . "Node::ConstChildNodeIterator")
    ("ConstObjectIterator" . "SceneNode::ConstObjectIterator")
    ("ConstShadowTextureConfigIterator" . "ConstShadowTextureConfigIterator")
    ("DataStreamPtr" . "DataStreamPtr")
    ("DataStreamListPtr" . "DataStreamListPtr")
    ("FileInfoListPtr" . "FileInfoListPtr")
    ("Light::LightTypes" . "Light::LightTypes")
    ("LightTypes" . "Light::LightTypes")
    ("Listener*" . "Node::Listener*")
    ("Matrix3" . "Matrix3")
    ("ManualObjectSection*" . "ManualObject::ManualObjectSection*")
    ("MeshPtr" . "MeshPtr")
    ("SceneManagerEnumerator::MetaDataIterator" . "SceneManagerEnumerator::MetaDataIterator")
    ("ObjectIterator" . "SceneNode::ObjectIterator")
    ("Overlay2DElementsIterator" . "Overlay::Overlay2DElementsIterator")
    ("OverlayMapIterator" . "OverlayManager::OverlayMapIterator")
    ("RenderOperation::OperationType" . "RenderOperation::OperationType")
    ("PlaneBoundedVolume" . "PlaneBoundedVolume")
    ("PrefabType" . "SceneManager::PrefabType")
    ("Ray" . "Ray")
    ("Real" . "Real")
    ("RenderSystemList*" . c-render-system-list)
    ("SceneManagerEnumerator::SceneManagerIterator" . "SceneManagerEnumerator::SceneManagerIterator")
    ("ShadowRenderableListIterator" . "Entity::ShadowRenderableListIterator")
    ("SpecialCaseRenderQueueMode" . "SceneManager::SpecialCaseRenderQueueMode")
    ("String" . "const char*")
    ("StringVector" . "StringVector")
    ("StringVectorPtr" . "StringVectorPtr")
    ("TemplateIterator" . "OverlayManager::TemplateIterator")
    ("TransformSpace" . "SceneNode::TransformSpace")
    ;("Vector3" . "Vector3")
    ("Vector3" . c-vector3)
    ("Vector4" . "Vector4")  ; XXX: this will lead to memory faults in CL
    ("VertexDataBindChoice" . "Entity::VertexDataBindChoice")
    ("ViewPoint" . "ViewPoint")
    ("const AxisAlignedBox&" . c-axis-aligned-box)
    ("const ColourValue&" . c-colour-value)
    ("const DisplayString&" . "const char*")
    ("const EntitySet*" . "const Entity::EntitySet*")
    ("const Matrix4&" . c-matrix4)
    ("const Plane&" . c-plane)
    ("const PlaneBoundedVolume&" . "const PlaneBoundedVolume&")
    ("const Quaternion&" . c-quaternion)
    ("const Radian&" . c-radian)
    ("const Sphere&" . c-sphere)
    ("const String&" . "const char*")
    ("const Vector3&" . c-vector3)
    ("const Vector3*" . "const Vector3*")
    ))
