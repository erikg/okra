;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; cpp-config.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Variables

(defparameter *cpp-build-dir* "generated/libokra/build/")
(defparameter *cpp-src-dir* "generated/libokra/src/")

(defparameter *cpp-files* nil)  ; for CMakeLists.txt

(defparameter *c-type-mapping*
  '(("void" . c-void)
    ("AnimationIterator" . c-animation-iterator)
    ("BoneAssignmentIterator" . c-bone-assignment-iterator)
    ("CameraIterator" . c-camera-iterator)
    ("ChildNodeIterator" . c-child-node-iterator)
    ("ChildObjectListIterator" . c-child-object-list-iterator)
    ("ConstChildNodeIterator" . c-const-child-node-iterator)
    ("ConstObjectIterator" .  c-const-object-iterator)
    ("ConstPoseIterator" . c-const-pose-iterator)
    ("LightTypes" . c-light-types)
    ("Listener*" . c-listener)
    ("LodDistanceIterator" . c-lod-distance-iterator)
    ("ManualObjectSection*" . c-manual-object-section)
    ("MeshPtr" . c-mesh-ptr)
    ("MovableObjectFactoryIterator" . c-movable-object-factory-iterator)
    ("MovableObjectIterator" . c-movable-object-iterator)
    ("ObjectIterator" . c-object-iterator)
    ("Overlay2DElementsIterator" . c-overlay-2d-elements-iterator)
    ("OverlayMapIterator" . c-overlay-map-iterator)
    ("PoseIterator" . c-pose-iterator)
    ("PrefabType" . c-prefab-type)
    ("RenderSystemList*" . c-render-system-list)
    ("ResourceDeclarationList" . c-resource-declaration-list)
    ("ResourceManagerIterator" . c-resource-manager-iterator)
    ("ShadowRenderableListIterator" . c-shadow-renderable-list-iterator)
    ("SpecialCaseRenderQueueMode" . c-special-case-render-queue-mode)
    ("String" . c-const-char)
    ("SubMeshIterator" . c-sub-mesh-iterator)
    ("TechniqueIterator" . c-technique-iterator)
    ("TemplateIterator" . c-template-iterator)
    ("TransformSpace" .  c-transform-space)
    ;("Vector3" . "Vector3")
    ("Vector3" . c-vector3)
    ("Vector4" . c-vector4)  ; XXX: this will lead to memory faults in CL
    ("VertexDataBindChoice" . c-vertex-data-bind-choice)
    ("const AxisAlignedBox&" . c-axis-aligned-box)
    ("const BoneBlendMask*" . c-bone-blend-mask)
    ("const ColourValue&" . c-colour-value)
    ("const DisplayString&" . c-const-char)
    ("const EntitySet*" . c-entity-set)
    ("const FactoryMap&" . c-factory-map)
    ("const FrameStats&" . c-frame-stats)
    ("const LodDistanceList&" . c-lod-distance-list)
    ("const Matrix4&" . c-matrix4)
    ("const Plane&" . c-plane)
    ("const PluginInstanceList&" . c-plugin-instance-list)
    ("const Quaternion&" . c-quaternion)
    ("const Radian&" . c-radian)
    ("const SkyBoxGenParameters&" . c-sky-box-gen-parameters)
    ("const SkyDomeGenParameters&" . c-sky-dome-gen-parameters)
    ("const SkyPlaneGenParameters&" . c-sky-plane-gen-parameters)
    ("const Sphere&" . c-sphere)
    ("const SubMeshNameMap&" . c-sub-mesh-name-map)
    ("const String&" . c-const-char)
    ("const Vector3&" . c-vector3)
    ("const VertexBoneAssignmentList&" . c-vertex-bone-assignment-list)))
