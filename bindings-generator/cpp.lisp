;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cpp.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Functions

;;; ARG-CODE, C-ARGS and CALL-ARGS should probably be merged.

(defun arg-code (args)
  (if (equal args "void")
      nil
      (iter (with result = "")
            (for arg in args)
            (as name = (cdr arg))
            (as type = (if (stringp (c-type (car arg)))
                           ""
                           (regex-replace-all "ARG"
                                 (funcall (c-type (car arg)) :arg-code) name)))
            (setf result (mkstr result (if type type "")))
            (finally (if (equal result "")
                         (return nil)
                         (return result))))))


(defun c-args (args &key (omit-name nil))
  (if (equal args "void")
      nil
      (iter (with result = "")
            (for arg in args)
            (as name = (cdr arg))
            ;; (funcall ... :arg-type) always returns a string
            (as type = (if (stringp (c-type (car arg)))
                           (c-type (car arg))
                           (funcall (c-type (car arg)) :arg-type)))
            (setf result (mkstr result ", " type (if omit-name
                                                     ""
                                                     (mkstr " " name))))
            (finally (return result)))))


(defun call-args (args)
  (iter (with result = "")
        (for arg in args)
        (for i from 0)
        (as name = (cdr arg))
        (as type = (if (stringp (c-type (car arg)))
                       nil
                       (funcall (c-type (car arg)) :arg-code)))
        (unless (= i 0)
          (setf result (mkstr result ", ")))
        (setf result (mkstr result (if type "ogre_" "") name))
        (finally (return result))))


(defun generate-c-fun (class-name member)
  (let* ((*print-pretty* nil)  ; for format below
         (args (getf member :args))
         (c-name (c-name (mkstr class-name "_" (getf member :name))))
         (type (c-type (getf member :type)))
         (return-type (if (stringp type) type (funcall type :return-type)))
         (return-type-arg nil))
    (unless (stringp return-type)
      (setf return-type-arg (mkstr (cadr return-type) " " (cddr return-type)))
      (setf return-type (car return-type)))
    (list :name (getf member :name)
          :type (format nil "~S" (getf member :type))
          :args (format nil "~S" (getf member :args))
          ;; make sure a const char* is returned (XXX: check for String!)
          :after-call (if (equal return-type "const char*") ".c_str()" nil)
          :arg-code (arg-code args)
          :before-call (unless (stringp type) (funcall type :before-call))
          :c-args (c-args args)
          :c-name c-name
          :call-args (call-args args)
          :class class-name
          :class-name (c-name class-name t)
          :return (if (or (equal return-type "void")
                          ;; see c-render-system-list
                          (unless (stringp type) (funcall type :no-return)))
                      nil
                      "return ")
          :return-type return-type
          :return-type-arg return-type-arg
          :post-code (unless (stringp type) (funcall type :post-code)))))


(defun generate-cmakelists (&optional (file-name "CMakeLists.txt"))
  (t2f *cpp-cmakelists-template* (mkstr *cpp-build-dir* file-name)
       (list :date (current-date-time-string)
             :file-name file-name
             :files (reverse *cpp-files*))))


(defun generate-cpp-bindings (&optional (classes *classes*))
  (iter (for (class-name nil) in-hashtable classes)
        (format t "Generating C++ bindings for ~A class...~%" class-name)
        (generate-cpp-file class-name))
  (format t "Generating C++ CMakeLists.txt...~%")
  (generate-cmakelists))


(defun generate-cpp-file (class-name &optional (extension ".cpp"))
  (let ((file-name (lisp-name (mkstr "ogre" class-name extension)))
        (members (gethash class-name *classes*)))
    (pushnew (list :file file-name) *cpp-files* :test 'equal)
    (t2f *cpp-file-template* (mkstr *cpp-src-dir* file-name)
         (list :date (current-date-time-string)
               :class-name class-name
               :file-name file-name
               :functions (iter (for member in (reverse members))
                                (when (blacklisted member)
                                  (next-iteration))
                                (when *verbose*
                                  (format t "Handling ~S...~%" member))
                                (collect (generate-c-fun class-name member)))
               :prototypes (generate-prototypes class-name members)))))


(defun generate-prototypes (class-name members)
  (iter (for member in (reverse members))
        (when (blacklisted member)
          (next-iteration))
        (as args = (getf member :args))
        (as c-name = (c-name (mkstr class-name "_" (getf member :name))))
        (as return-type = (if (stringp (c-type (getf member :type)))
                              (c-type (getf member :type))
                              (funcall (c-type (getf member :type))
                                       :return-type)))
        (as return-type-arg = (if (stringp return-type)
                                  nil
                                  (cadr return-type)))
        (unless (stringp return-type)
          (setf return-type (car return-type)))
        (collect (list :return-type return-type
                       :name c-name
                       :class class-name
                       :return-type-arg return-type-arg
                       :args (c-args args :omit-name t)))))


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

(defun c-axis-aligned-box (type)
  (case type
    (:arg-code (mkstr "AxisAlignedBox ogre_ARG = AxisAlignedBox(ARG[0], "
                      "ARG[1], ARG[2], ARG[3], ARG[4], ARG[5]);"))
    (:arg-type "const okraArray6")
    (:return-type "const AxisAlignedBox&")
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


(defun c-plane (type)
  (case type
    (:arg-type "const okraArray4")
    (:arg-code "Plane ogre_ARG = Plane(ARG[0], ARG[1], ARG[2], ARG[3]);")
    (:return-type "const Plane&")
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
    (:arg-type "Real")
    (:before-call "Radian ogre_r = ")
    (:arg-code "Radian ogre_ARG = Radian(ARG);")
    (:post-code "return ogre_r.valueRadians();")
    (:no-return t)
    (:return-type "okraReal")
    (otherwise nil)))


(defun c-ray (type)
  (case type
    (:arg-type "const okraArray6")
    (:arg-code (mkstr "Ray ogre_ARG = Ray(Vector3(ARG[0], ARG[1], ARG[2]), "
                      "Vector3(ARG[3], ARG[4], ARG[5]));"))
    (:return-type "Ray*")
    (otherwise nil)))


;;; First try to return a CPP vector as a dynamically sized array.  Leaks
;;; like a sieve in combination with cl-render-system-list.
(defun c-render-system-list (type)
  (case type
    (:before-call "RenderSystemList* rsl = ")
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
    (:no-return t)
    (:return-type "const char**")
    (otherwise nil)))


(defun c-sphere (type)
  (case type
    (:arg-type "const okraArray4")
    (:arg-code (mkstr "Sphere ogre_ARG = Sphere(Vector3(ARG[0], ARG[1], "
                      "ARG[2]), ARG[3]);"))
    (:return-type "const Sphere&")
    (otherwise nil)))


(defun c-vector3 (type)
  (case type
    (:arg-type "const okraArray3")
    (:arg-code "Vector3 ogre_ARG = Vector3(ARG[0], ARG[1], ARG[2]);")
    (:before-call "Vector3 ogre_v = ")
    (:post-code (mkfstr "v[0] = ogre_v[0];~%    v[1] = ogre_v[1];~%"
                    "    v[2] = ogre_v[2];"))
    (:return-type '("void" . ("okraArray3" . "v")))
    (otherwise nil)))
