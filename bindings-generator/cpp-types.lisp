;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cpp-types.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Type Handlers

;;;## Type Handler Keywords

;;;### :arg-code
;;;
;;; Basically translates an argument passed to the C wrapper to an
;;; argument suitable for Ogre.  If you check C-AXIS-ALIGNED-BOX
;;; you'll see that its argument type (:arg-type) for the C-wrapper
;;; function is "const okraArray6" and that the code defined in
;;; :arg-code creates an Ogre::AxisAlignedBox from that.

;;;### :arg-type
;;;
;;; The argument type it should take in the defcfun.

;;;### :before-call
;;;
;;; Code that precedes the call to the cfun (on the same line).

;;;### :no-return
;;;
;;; It is generally deduced from the :return-type line whether a
;;; "return " is put in front of the cfun call.  This doesn't always
;;; work correctly so you can specify here that no "return " should be
;;; generated.  This is a bit of a hack.

;;;### :post-code
;;;
;;; Code that comes on the line after the call to the cfun.

;;;### :return-arg
;;;
;;; Ogre returns Vector3, Quaternions and other objects.  I have
;;; chosen to pass a CFFI allocated array by reference to the wrapper
;;; functions and have that array filled with the relevant values of
;;; the returned object.  If you check the CL-ARRAY3 function below
;;; you'll see that the :below line allocates the array and
;;; :return-arg makes sure that what was actually a returned object by
;;; Ogre is now the name of the array reference.

;;;### :return-type
;;;
;;; This is either a string or a cons.  If it is a string it just
;;; signifies what the return type of the wrapper function should be.
;;; If it is a cons then its car signifies the return type of the
;;; wrapper function (generally bool or void) and its cdr signifies
;;; that we should also generate an argument for the C wrapper
;;; function.  This is usually to pass an array by reference instead
;;; of returning a pointer. Take CL-VECTOR3 as an example, this is
;;; used for Ogre functions that return a Vector3.  Instead of
;;; returning a pointer to a Vector3 object we get passed a reference
;;; to an array created by CFFI that can hold 3 Ogre::Reals.  We
;;; specify its type and its name in the cdr and fill it using the
;;; code in :post-code.

;;;## Type Handler Functions

(defun c-animation-iterator (type)
  (case type
    (:arg-type "SceneManager::AnimationIterator")
    (:return-type "SceneManager::AnimationIterator")
    (otherwise nil)))


(defun c-axis-aligned-box (type)
  (case type
    (:arg-code (mkstr "AxisAlignedBox ogre_ARG = AxisAlignedBox(ARG[0], "
                      "ARG[1], ARG[2], ARG[3], ARG[4], ARG[5]);"))
    (:arg-type "const okraArray6")
    (:return-type "const AxisAlignedBox&")
    (otherwise nil)))


(defun c-bone-blend-mask (type)
  (case type
    (:arg-type "const AnimationState::BoneBlendMask*")
    (:return-type "const AnimationState::BoneBlendMask*")
    (otherwise nil)))


(defun c-camera-iterator (type)
  (case type
    (:arg-type "SceneManager::CameraIterator")
    (:return-type "SceneManager::CameraIterator")
    (otherwise nil)))


(defun c-const-char (type)
  (case type
    (:arg-type "const char*")
    (:return-type "const char*")
    (otherwise nil)))


;;; XXX: I really need to figure out how to get rid of these types that need
;;; XXX: a class in front of them.  What's different in my C code from the
;;; XXX: of regular code that uses Ogre?
(defun c-child-node-iterator (type)
  (case type
    (:arg-type "Node::ChildNodeIterator")
    (:return-type "Node::ChildNodeIterator")
    (otherwise nil)))


(defun c-child-object-list-iterator (type)
  (case type
    (:arg-type "Entity::ChildObjectListIterator")
    (:return-type "Entity::ChildObjectListIterator")
    (otherwise nil)))


(defun c-const-child-node-iterator (type)
  (case type
    (:arg-type "Node::ConstChildNodeIterator")
    (:return-type "Node::ConstChildNodeIterator")
    (otherwise nil)))


(defun c-const-object-iterator (type)
  (case type
    (:arg-type "SceneNode::ConstObjectIterator")
    (:return-type "SceneNode::ConstObjectIterator")
    (otherwise nil)))


(defun c-colour-value (type)
  (case type
    (:arg-type "const okraArray4")
    (:arg-code (mkstr "ColourValue ogre_ARG = ColourValue(ARG[0], ARG[1], "
                      "ARG[2], ARG[3]);"))
    (:before-call "ColourValue ogre_cv = ")
    (:post-code (mkfstr "cv[0] = ogre_cv.r;~%    cv[1] = ogre_cv.g;~%"
                    "    cv[2] = ogre_cv.b;~%    cv[3] = ogre_cv.a;"))
    (:return-type '("void" . ("okraArray4" . "cv")))
    (otherwise nil)))


(defun c-entity-set (type)
  (case type
    (:arg-type "const Entity::EntitySet*")
    (:return-type "const Entity::EntitySet*")
    (otherwise nil)))


(defun c-factory-map (type)
  (case type
    (:arg-type "const OverlayManager::FactoryMap&")
    (:return-type "const OverlayManager::FactoryMap&")
    (otherwise nil)))


(defun c-frame-stats (type)
  (case type
    (:arg-type "const RenderTarget::FrameStats&")
    (:return-type "const RenderTarget::FrameStats&")
    (otherwise nil)))


(defun c-light-types (type)
  (case type
    (:arg-type "Light::LightTypes")
    (:return-type "Light::LightTypes")
    (otherwise nil)))


(defun c-listener (type)
  (case type
    (:arg-type "Node::Listener*")
    (:return-type "Node::Listener*")
    (otherwise nil)))


(defun c-lod-distance-iterator (type)
  (case type
    (:arg-type "Material::LodDistanceIterator")
    (:return-type "Material::LodDistanceIterator")
    (otherwise nil)))


(defun c-lod-distance-list (type)
  (case type
    (:arg-type "const Material::LodDistanceList&")
    (:return-type "const Material::LodDistanceList&")
    (otherwise nil)))


(defun c-manual-object-section (type)
  (case type
    (:arg-type "ManualObject::ManualObjectSection*")
    (:return-type "ManualObject::ManualObjectSection*")
    (otherwise nil)))


(defun c-matrix4 (type)
  (case type
    (:arg-code (mkstr "Matrix4 ogre_ARG = Matrix4(ARG[0], ARG[1], ARG[2], "
                      "ARG[3], ARG[4], ARG[5], ARG[6], ARG[7], ARG[8], "
                      "ARG[9], ARG[10], ARG[11], ARG[12], ARG[13], ARG[14], "
                      "ARG[15]);"))
    (:arg-type "const okraArray16")
    (:before-call "Matrix4 ogre_m4 = ")
    (:post-code (mkfstr "m4[ 0] = *(ogre_m4[ 0]);  m4[ 1] = *(ogre_m4[ 1]);~%"
                    "    m4[ 2] = *(ogre_m4[ 2]);  m4[ 3] = *(ogre_m4[ 3]);~%"
                    "    m4[ 4] = *(ogre_m4[ 4]);  m4[ 5] = *(ogre_m4[ 5]);~%"
                    "    m4[ 6] = *(ogre_m4[ 6]);  m4[ 7] = *(ogre_m4[ 7]);~%"
                    "    m4[ 8] = *(ogre_m4[ 8]);  m4[ 9] = *(ogre_m4[ 9]);~%"
                    "    m4[10] = *(ogre_m4[10]);  m4[11] = *(ogre_m4[11]);~%"
                    "    m4[12] = *(ogre_m4[12]);  m4[13] = *(ogre_m4[13]);~%"
                    "    m4[14] = *(ogre_m4[14]);  m4[15] = *(ogre_m4[15]);"))
    (:return-type '("void" . ("okraArray16" . "m4")))
    (otherwise nil)))


(defun c-movable-object-factory-iterator (type)
  (case type
    (:arg-type "Root::MovableObjectFactoryIterator")
    (:return-type "Root::MovableObjectFactoryIterator")
    (otherwise nil)))


(defun c-movable-object-iterator (type)
  (case type
    (:arg-type "SceneManager::MovableObjectIterator")
    (:return-type "SceneManager::MovableObjectIterator")
    (otherwise nil)))


(defun c-object-iterator (type)
  (case type
    (:arg-type "SceneNode::ObjectIterator")
    (:return-type "SceneNode::ObjectIterator")
    (otherwise nil)))


(defun c-overlay-2d-elements-iterator (type)
  (case type
    (:arg-type "Overlay::Overlay2DElementsIterator")
    (:return-type "Overlay::Overlay2DElementsIterator")
    (otherwise nil)))


(defun c-overlay-map-iterator (type)
  (case type
    (:arg-type  "OverlayManager::OverlayMapIterator")
    (:return-type  "OverlayManager::OverlayMapIterator")
    (otherwise nil)))


(defun c-prefab-type (type)
  (case type
    (:arg-type "SceneManager::PrefabType")
    (:return-type "SceneManager::PrefabType")
    (otherwise nil)))


(defun c-plane (type)
  (case type
    (:arg-code "Plane ogre_ARG = Plane(ARG[0], ARG[1], ARG[2], ARG[3]);")
    (:arg-type "const okraArray4")
    (:return-type "const Plane&")
    (otherwise nil)))


(defun c-plugin-instance-list (type)
  (case type
    (:arg-type "const Root::PluginInstanceList&")
    (:return-type "const Root::PluginInstanceList&")
    (otherwise nil)))


(defun c-quaternion (type)
  (case type
    (:arg-code (mkstr "Quaternion ogre_ARG = Quaternion(ARG[0], ARG[1], "
                      "ARG[2], ARG[3]);"))
    (:arg-type "const okraArray4")
    (:before-call "Quaternion ogre_q = ")
    (:post-code (mkfstr "q[0] = ogre_q[0];~%    q[1] = ogre_q[1];~%"
                    "    q[2] = ogre_q[2];~%    q[3] = ogre_q[3];"))
    (:return-type '("void" . ("okraArray4" . "q")))
    (otherwise nil)))


(defun c-radian (type)
  (case type
    (:arg-code "Radian ogre_ARG = Radian(ARG);")
    (:arg-type "Real")
    (:before-call "Radian ogre_r = ")
    (:no-return t)
    (:post-code "return ogre_r.valueRadians();")
    (:return-type "okraReal")
    (otherwise nil)))


(defun c-ray (type)
  (case type
    (:arg-code (mkstr "Ray ogre_ARG = Ray(Vector3(ARG[0], ARG[1], ARG[2]), "
                      "Vector3(ARG[3], ARG[4], ARG[5]));"))
    (:arg-type "const okraArray6")
    (:return-type "Ray*")
    (otherwise nil)))


;;; First try to return a CPP vector as a dynamically sized array.  Leaks
;;; like a sieve in combination with cl-render-system-list.
(defun c-render-system-list (type)
  (case type
    (:before-call "RenderSystemList* rsl = ")
    (:no-return t)
    (:post-code
      (mkfstr "const char** list = new const char*[rsl->size()+1];~%~%"
              "    String size = StringConverter::toString(rsl->size());~%"
              "    list[0] = new char[size.length()];~%"
              "    list[0] = size.c_str();~%~%"
              "    for (size_t i = 0; i < rsl->size(); i++)~%    {~%"
              "        RenderSystem* rs = rsl->at(i);~%"
              "        String name = rs->getName();~%"
              "        list[i+1] = new char[name.length()];~%"
              "        list[i+1] = name.c_str();~%    }~%~%"
              "    return list;"))
    (:return-type "const char**")
    (otherwise nil)))


(defun c-resource-declaration-list (type)
  (case type
    (:arg-type "ResourceGroupManager::ResourceDeclarationList")
    (:return-type "ResourceGroupManager::ResourceDeclarationList")
    (otherwise nil)))


(defun c-resource-manager-iterator (type)
  (case type
    (:arg-type "ResourceGroupManager::ResourceManagerIterator")
    (:return-type "ResourceGroupManager::ResourceManagerIterator")
    (otherwise nil)))


(defun c-shadow-renderable-list-iterator (type)
  (case type
    (:arg-type "Entity::ShadowRenderableListIterator")
    (:return-type "Entity::ShadowRenderableListIterator")
    (otherwise nil)))


(defun c-sky-box-gen-parameters (type)
  (case type
    (:arg-type "SceneManager::SkyBoxGenParameters")
    (:return-type "SceneManager::SkyBoxGenParameters")
    (otherwise nil)))


(defun c-sky-dome-gen-parameters (type)
  (case type
    (:arg-type "SceneManager::SkyDomeGenParameters")
    (:return-type "SceneManager::SkyDomeGenParameters")
    (otherwise nil)))


(defun c-sky-plane-gen-parameters (type)
  (case type
    (:arg-type "SceneManager::SkyPlaneGenParameters")
    (:return-type "SceneManager::SkyPlaneGenParameters")
    (otherwise nil)))


(defun c-special-case-render-queue-mode (type)
  (case type
    (:arg-type "SceneManager::SpecialCaseRenderQueueMode")
    (:return-type "SceneManager::SpecialCaseRenderQueueMode")
    (otherwise nil)))


(defun c-sphere (type)
  (case type
    (:arg-code (mkstr "Sphere ogre_ARG = Sphere(Vector3(ARG[0], ARG[1], "
                      "ARG[2]), ARG[3]);"))
    (:arg-type "const okraArray4")
    (:return-type "const Sphere&")
    (otherwise nil)))


(defun c-technique-iterator (type)
  (case type
    (:arg-type "Material::TechniqueIterator")
    (:return-type "Material::TechniqueIterator")
    (otherwise nil)))


(defun c-template-iterator (type)
  (case type
    (:arg-type "OverlayManager::TemplateIterator")
    (:return-type "OverlayManager::TemplateIterator")
    (otherwise nil)))


(defun c-transform-space (type)
  (case type
    (:arg-type "SceneNode::TransformSpace")
    (:return-type "SceneNode::TransformSpace")
    (otherwise nil)))


(defun c-uint16-ptr (type)
  (case type
    (:arg-type "const uint16*")
    (:return-type "const uint16*")
    (otherwise nil)))


(defun c-vector3 (type)
  (case type
    (:arg-code "Vector3 ogre_ARG = Vector3(ARG[0], ARG[1], ARG[2]);")
    (:arg-type "const okraArray3")
    (:before-call "Vector3 ogre_v = ")
    (:post-code (mkfstr "v[0] = ogre_v[0];~%    v[1] = ogre_v[1];~%"
                    "    v[2] = ogre_v[2];"))
    (:return-type '("void" . ("okraArray3" . "v")))
    (otherwise nil)))


(defun c-vector4 (type)
  (case type
    (:arg-type "Vector4")
    (:return-type "Vector4")
    (otherwise nil)))


(defun c-vertex-data-bind-choice (type)
  (case type
    (:arg-type "Entity::VertexDataBindChoice")
    (:return-type "Entity::VertexDataBindChoice")
    (otherwise nil)))


(defun c-void (type)
  (case type
    (:return-type "void")
    (otherwise nil)))
