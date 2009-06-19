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
(defparameter *typedefs* (make-hash-table :test 'equal))

;;; Bindings should be generated for the following files.  Maybe at some
;;; indefinite time in the future we'll be able to process all files.
;;; (I doubt it and I'm not even sure we'd want to do that.)
(defparameter *doxygen-files*
  '("xml/class_ogre_1_1_camera.xml"
    "xml/class_ogre_1_1_entity.xml"
    "xml/class_ogre_1_1_frustum.xml"
    "xml/class_ogre_1_1_light.xml"
    "xml/class_ogre_1_1_manual_object.xml"
    "xml/class_ogre_1_1_movable_object.xml"
    "xml/class_ogre_1_1_node.xml"
    "xml/class_ogre_1_1_overlay.xml"
    "xml/class_ogre_1_1_overlay_element.xml"
    "xml/class_ogre_1_1_overlay_manager.xml"
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

(defparameter *doxygen-enums-files* (append1 *doxygen-files*
                                             "xml/namespace_ogre.xml"))
(defparameter *doxygen-typedefs-files* (append1 *doxygen-files*
                                                "xml/namespace_ogre.xml"))

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
(defparameter *cl-asd-template* (create-template-printer #p"cl-asd.t"))
(defparameter *cl-enums-template* (create-template-printer #p"cl-enums.t"))
(defparameter *cl-file-template* (create-template-printer #p"cl-file.t"))
(defparameter *cl-package-template* (create-template-printer #p"cl-package.t"))
(defparameter *cl-typedefs-template*
              (create-template-printer #p"cl-typedefs.t"))
(defparameter *cpp-file-template* (create-template-printer #p"cpp-file.t"))
(defparameter *cpp-cmakelists-template*
              (create-template-printer #p"cpp-cmakelists.t"))

;;; *blacklist* is basically a dump for members that I don't want to or don't
;;; know how to deal with.
(defparameter *blacklist*
  '(
    (:name "addListener" :type "void" :args (("Listener*" . "s")))
    ;; preferring ColourValue version
    (:name "colour" :type "void" :args (("Real" . "r") ("Real" . "g") ("Real" . "b") ("Real" . "a")))
    (:name "copyContentsToMemory" :type "void" :args (("const PixelBox&" . "dst") ("FrameBuffer" . "buffer")))
    ;; arbitrarily preferring RenderWindow version
    (:name "create" :type "Overlay*" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    (:name "createChild" :type "Node*" :args (("const Vector3&" . "translate") ("const Quaternion&" . "rotate")))
    ;; arbitrarily preferring the string version
    (:name "createChildSceneNode" :type "SceneNode*" :args (("const Vector3&" . "translate") ("const Quaternion&" . "rotate")))
    ;; preferring the enum version
    (:name "createEntity" :type "Entity*" :args (("const String&" . "entityName") ("const String&" . "meshName")))
    ;; arbitrary choice
    (:name "createParticleSystem" :type "ParticleSystem*" :args (("const String&" . "name") ("const String&" . "templateName")))
    ;; should try this one out instead of the string version
    (:name "createSceneManager" :type "SceneManager*" :args (("SceneTypeMask" . "typeMask") ("const String&" . "instanceName")))
    ;; arbitrarily preferring the string version
    (:name "createSceneNode" :type "SceneNode*" :args "void")
    ;; arbitrary choice
    (:name "declareResource" :type "void" :args (("const String&" . "name") ("const String&" . "resourceType") ("const String&" . "groupName") ("ManualResourceLoader*" . "loader") ("const NameValuePairList&" . "loadParameters")))
    ;; arbitrarily preferring RenderWindow version
    (:name "destroy" :type "void" :args (("Overlay*" . "overlay")))
    ;; preferring the pointer version
    (:name "destroy" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyBillboardChain" :type "void" :args (("BillboardChain*" . "obj")))
    (:name "destroyBillboardChain" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyBillboardSet" :type "void" :args (("BillboardSet*" . "set")))
    (:name "destroyBillboardSet" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyCamera" :type "void" :args (("Camera*" . "cam")))
    (:name "destroyCamera" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyEntity" :type "void" :args (("Entity*" . "ent")))
    (:name "destroyEntity" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyInstancedGeometry" :type "void" :args (("InstancedGeometry*" . "geom")))
    (:name "destroyInstancedGeometry" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyLight" :type "void" :args (("Light*" . "light")))
    (:name "destroyLight" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyManualObject" :type "void" :args (("ManualObject*" . "obj")))
    (:name "destroyManualObject" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyMovableObject" :type "void" :args (("MovableObject*" . "m")))
    (:name "destroyMovableObject" :type "void" :args (("const String&" . "name") ("const String&" . "typeName")))
    ;; preferring the pointer version
    (:name "destroyOverlayElement" :type "void" :args (("const String&" . "instanceName") ("bool" . "isTemplate")))
    ;; blacklisting with extreme prejudice
    (:name "getOverlayElementFactoryMap" :type "const FactoryMap&" :args NIL)
    ;; arbitrarily preferring the string version
    ;(:name "destroyParticleSystem" :type "void" :args (("ParticleSystem*" . "obj")))
    (:name "destroyParticleSystem" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    (:name "detachRenderTarget" :type "void" :args (("RenderTarget*" . "pWin")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyRibbonTrail" :type "void" :args (("RibbonTrail*" . "obj")))
    (:name "destroyRibbonTrail" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroySceneNode" :type "void" :args (("SceneNode*" . "sn")))
    (:name "destroySceneNode" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    ;(:name "destroyStaticGeometry" :type "void" :args (("StaticGeometry*" . "geom")))
    (:name "destroyStaticGeometry" :type "void" :args (("const String&" . "name")))
    ;; arbitrarily preferring the string version
    (:name "detachObject" :type "MovableObject*" :args (("unsigned short" . "index")))
    (:name "detachObject" :type "void" :args (("MovableObject*" . "obj")))
    ;; arbitrarily preferring the string version
    (:name "detachObjectFromBone" :type "MovableObject*" :args (("const String&" . "movableName")))
    ;; arbitrary choice
    (:name "enableCustomNearClipPlane" :type "void" :args (("const Plane&" . "plane")))
    ;; arbitrary choice
    (:name "enableReflection" :type "void" :args (("const Plane&" . "p")))
    ;; arbitrarily preferring the string version
    (:name "estimateWorldGeometry" :type "size_t" :args (("DataStreamPtr&" . "stream") ("const String&" . "typeName")))
    ;; arbitrarily preferring the string version
    (:name "extractMovableObject" :type "void" :args (("MovableObject*" . "m")))
    (:name "forwardIntersect" :type "void" :args (("const Plane&" . "worldPlane") ("std::vector< Vector4 >*" . "intersect3d")))
    (:name "getAnimationIterator" :type "AnimationIterator" :args "void")
    ;; arbitrarily preferring the string version
    (:name "getAttachedObject" :type "MovableObject*" :args (("unsigned short" . "index")))
    ;; arbitrary choice
    (:name "getAttachedObjectIterator" :type "ConstObjectIterator" :args "void")
    ;; preferring the Viewport one
    (:name "getCamera" :type "Camera*" :args (("const String&" . "name")))
    (:name "getCameraIterator" :type "CameraIterator" :args "void")
    (:name "getCameraToViewportBoxVolume" :type "void" :args (("Real" . "screenLeft") ("Real" . "screenTop") ("Real" . "screenRight") ("Real" . "screenBottom") ("PlaneBoundedVolume*" . "outVolume") ("bool" . "includeFarPlane")))
    (:name "getCameraToViewportRay" :type "void" :args (("Real" . "screenx") ("Real" . "screeny") ("Ray*" . "outRay")))
    ;; returns an uint16* (UTF char)
    (:name "getCaption" :type "const DisplayString&" :args "void")
    ;; arbitrarily preferring the string version
    (:name "getChild" :type "Node*" :args (("unsigned short" . "index")))
    ;; arbitrary choice
    (:name "getChildIterator" :type "ConstChildNodeIterator" :args "void")
    (:name "getDerivedPosition" :type "const Vector3&" :args (("bool" . "cameraRelativeIfSet")))
    ;; dunno what to do with the type (wtf is :args nil?)
    (:name "getInstalledPlugins" :type "const PluginInstanceList&" :args nil)
    (:name "getListener" :type "Listener*" :args "void")
    ;; arbitrary choice for RenderTarget version
    (:name "getMetrics" :type "void" :args (("unsigned int&" . "width") ("unsigned int&" . "height") ("unsigned int&" . "colourDepth") ("int&" . "left") ("int&" . "top")))
    (:name "getMovableObjectFactoryIterator" :type "MovableObjectFactoryIterator" :args "void")
    (:name "getMovableObjectIterator" :type "MovableObjectIterator" :args (("const String&" . "typeName")))
    (:name "getQueuedRenderableVisitor" :type "SceneMgrQueuedRenderableVisitor*" :args "void")
    (:name "getResourceDeclarationList" :type "ResourceDeclarationList" :args (("const String&" . "groupName")))
    (:name "getResourceManagerIterator" :type "ResourceManagerIterator" :args NIL)
    ;; used in Camera but different from the one in Root
    (:name "getSceneManager" :type "SceneManager*" :args "void")
    ;; we need the handwritten version
    ;(:name "getSingletonPtr" :type "ResourceGroupManager*" :args "void")
    (:name "getSkyBoxGenParameters" :type "const SkyBoxGenParameters&" :args "void")
    (:name "getSkyDomeGenParameters" :type "const SkyDomeGenParameters&" :args "void")
    (:name "getSkyPlaneGenParameters" :type "const SkyPlaneGenParameters&" :args "void")
    (:name "getSpecialCaseRenderQueueMode" :type "SpecialCaseRenderQueueMode" :args "void")
    (:name "getStatistics" :type "const FrameStats&" :args "void")
    ;; arbitrarily preferring the string version
    (:name "getSubEntity" :type "SubEntity*" :args (("unsigned int" . "index")))
    ;; dunno why it doesn't work
    (:name "getViewMatrix" :type "const Matrix4&" :args "void")
    ;; preferring the member in Viewport
    (:name "getViewport" :type "Viewport*" :args "void")
    ;; no support for std::vector yet
    (:name "getWindowPlanes" :type "const std::vector< Plane >&" :args "void")
    ;; the Root version is required
    (:name "initialise" :type "void" :args "void")
    ;; arbitrary choice for Vector3 version
    (:name "isVisible" :type "bool" :args "void")
    ;; arbitrary choice for Vector3 version
    (:name "isVisible" :type "bool" :args (("const AxisAlignedBox&" . "bound") ("FrustumPlane*" . "culledBy")))
    ;; arbitrary choice for Vector3 version
    (:name "isVisible" :type "bool" :args (("const Sphere&" . "bound") ("FrustumPlane*" . "culledBy")))
    ;; preferring lookAt(Vector3)
    (:name "lookAt" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    ;; interferes with a more generic version
    (:name "lookAt" :type "void" :args (("const Vector3&" . "targetPoint") ("TransformSpace" . "relativeTo") ("const Vector3&" . "localDirectionVector")))
    ;; preferring Vector3 version
    (:name "normal" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    ;; interferes with more generic function
    (:name "pitch" :type "void" :args (("const Radian&" . "angle") ("TransformSpace" . "relativeTo")))
    ;; preferring Vector3 version
    (:name "position" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    ;; arbitrarily preferring the string version
    (:name "prepareWorldGeometry" :type "void" :args (("DataStreamPtr&" . "stream") ("const String&" . "typeName")))
    ;; no idea how to handle this one
    (:name "projectSphere" :type "bool" :args (("const Sphere&" . "sphere") ("Real*" . "left") ("Real*" . "top") ("Real*" . "right") ("Real*" . "bottom")))
    ;; arbitrarily preferring the string version
    (:name "removeAndDestroyChild" :type "void" :args (("unsigned short" . "index")))
    ;; arbitrarily preferring the string version
    (:name "removeChild" :type "Node*" :args (("unsigned short" . "index")))
    (:name "removeChild" :type "Node*" :args (("Node*" . "child")))
    (:name "removeListener" :type "void" :args (("Listener*" . "s")))
    (:name "resourceExists" :type "bool" :args (("ResourceGroup*" . "group") ("const String&" . "filename")))
    (:name "resourceModifiedTime" :type "time_t" :args (("ResourceGroup*" . "group") ("const String&" . "filename")))
    ;; interferes with more generic function
    (:name "roll" :type "void" :args (("const Radian&" . "angle") ("TransformSpace" . "relativeTo")))
    ;; interferes with more generic function
    (:name "rotate" :type "void" :args (("const Quaternion&" . "q") ("TransformSpace" . "relativeTo")))
    ;; preferring rotate(Quaternion)
    (:name "rotate" :type "void" :args (("const Vector3&" . "axis") ("const Radian&" . "angle")))
    (:name "rotate" :type "void" :args (("const Vector3&" . "axis") ("const Radian&" . "angle") ("TransformSpace" . "relativeTo")))
    ;; preferring scale(Vector3)
    (:name "scale" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    ;; arbitrary choice
    (:name "setAutoTracking" :type "void" :args (("bool" . "enabled") ("SceneNode*" . "target") ("const Vector3&" . "localDirectionVector") ("const Vector3&" . "offset")))
    ;; arbitrary choice
    (:name "setDebugDisplayEnabled" :type "void" :args (("bool" . "enabled") ("bool" . "cascade")))
    ;; preferring setDiffuseColour(const ColourValue&) since functions return
    ;; values in that format as well
    (:name "setDiffuseColour" :type "void" :args (("Real" . "red") ("Real" . "green") ("Real" . "blue")))
    ;; OverlayElement version preferred
    (:name "setDimensions" :type "void" :args (("Real" . "left") ("Real" . "top") ("Real" . "width") ("Real" . "height")))
    ;; preferring setDirection(Vector3)
    (:name "setDirection" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    (:name "setDirection" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z") ("TransformSpace" . "relativeTo") ("const Vector3&" . "localDirectionVector")))
    ;; interferes with a more generic version
    (:name "setDirection" :type "void" :args (("const Vector3&" . "vec") ("TransformSpace" . "relativeTo") ("const Vector3&" . "localDirectionVector")))
    ;; preferring setFrustumOffset(Vector2)
    (:name "setFrustumOffset" :type "void" :args (("Real" . "horizontal") ("Real" . "vertical")))
    (:name "setListener" :type "void" :args (("Listener*" . "listener")))
    ;; conflicts with more generic definition
    (:name "setMaterialName" :type "void" :args (("size_t" . "subindex") ("const String&" . "name")))
    ;; preferring setOrientation(Quaternion)
    (:name "setOrientation" :type "void" :args (("Real" . "w") ("Real" . "x") ("Real" . "y") ("Real" . "z")))
    ;; from OverlayElement
    (:name "setPosition" :type "void" :args (("Real" . "left") ("Real" . "top")))
    ;; preferring setPosition(Vector3)
    (:name "setPosition" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    (:name "setQueuedRenderableVisitor" :type "void" :args (("SceneMgrQueuedRenderableVisitor*" . "visitor")))
    (:name "setScale" :type "void" :args (("Real" . "x") ("Real" . "y")))
    ;; preferring setScale(Vector3)
    (:name "setScale" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z")))
    (:name "setShadowTextureConfig" :type "void" :args (("size_t" . "shadowIndex") ("const ShadowTextureConfig&" . "config")))
    ;; preferring setSpecualrColour(const ColourValue&) since functions return
    ;; values in that format as well
    (:name "setSpecularColour" :type "void" :args (("Real" . "red") ("Real" . "green") ("Real" . "blue")))
    ;; interferes with a more generic version
    (:name "setVisible" :type "void" :args (("bool" . "visible") ("bool" . "cascade")))
    ;; arbitrarily preferring the string version
    (:name "setWorldGeometry" :type "void" :args (("DataStreamPtr&" . "stream") ("const String&" . "typeName")))
    ;; preferring Vector2 version
    (:name "textureCoord" :type "void" :args (("Real" . "u")))
    (:name "textureCoord" :type "void" :args (("Real" . "u") ("Real" . "v")))
    (:name "textureCoord" :type "void" :args (("Real" . "u") ("Real" . "v") ("Real" . "w")))
    (:name "textureCoord" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z") ("Real" . "w")))
    (:name "textureCoord" :type "void" :args (("const Vector3&" . "uvw")))
    (:name "textureCoord" :type "void" :args (("const Vector4&" . "xyzw")))
    ;; "Ignore what you don't understand."
    (:name "translate" :type "void" :args (("Real" . "x") ("Real" . "y") ("Real" . "z") ("TransformSpace" . "relativeTo")))
    (:name "translate" :type "void" :args (("const Matrix3&" . "axes") ("const Vector3&" . "move") ("TransformSpace" . "relativeTo")))
    (:name "translate" :type "void" :args (("const Matrix3&" . "axes") ("Real" . "x") ("Real" . "y") ("Real" . "z") ("TransformSpace" . "relativeTo")))
    (:name "update" :type "void" :args "void")
    ;; interferes with a more generic version
    (:name "yaw" :type "void" :args (("const Radian&" . "angle") ("TransformSpace" . "relativeTo")))
    ))
