;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; common-lisp.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Functions

(defun add-lisp-symbol (name)
  (pushnew (list :symbol (string-downcase name)) *lisp-symbols* :test 'equal))


(defun base-class-name (class-name)
  (let ((base-class-name (gethash class-name *base-classes*)))
    (cond ((and base-class-name
                ;; are we generating wrappers for this base-class?
                (gethash base-class-name *classes*))
           (lisp-name base-class-name))
          ;; in case we don't have this class' base-class
          (t "ogre-object"))))


(defun cl-args (args)
  (iter (for arg in args)
        (as name = (lisp-name (cdr arg)))
        (as type = (if (stringp (lisp-type (car arg)))
                       (lisp-type (car arg))
                       (funcall (lisp-type (car arg)) :arg-type)))
        (collect (list :arg-name name :arg-type type))))


(defun generate-asdf-file (&optional (file-name "okra-bindings.asd"))
  ;(t2f *cl-asd-template* (mkstr "generated/" file-name)
  (t2f *cl-asd-template* (mkstr "../" file-name)
       (list :date (current-date-time-string)
             :file-name file-name
             :files *lisp-files*)))


(defun generate-enums-file (&key (file-name "enums") (extension ".lisp"))
  (pushnew (list :file file-name) *lisp-files* :test 'equal)
  (t2f *cl-enums-template* (mkstr *lisp-out-dir* file-name extension)
       (list :date (current-date-time-string)
             :file-name (mkstr file-name extension)
             :enum-types
             (iter (for (enum enums) in-hashtable *enum-types*)
                   (as type-name = (lisp-name enum))
                   (collect
                     (list :type-name type-name
                           :enums
                           (iter (for name in (reverse enums))
                                 (collect
                                   (list :name
                                         (lisp-name (string-downcase name))
                                         :type (gethash name *enums*))))))))))


(defun generate-lisp-bindings (&optional (classes *classes*))
  (loop for name being the hash-keys in classes
        do (format t "Generating Common Lisp bindings for ~A class...~%" name)
           (generate-lisp-file name))
  (format t "Generating Common Lisp enums file...~%")
  (generate-enums-file)
  (format t "Generating Common Lisp typedefs file...~%")
  (generate-typedefs-file)
  (format t "Generating Common Lisp package file...~%")
  (generate-package-file)
  (format t "Generating Common Lisp ASDF file...~%")
  (generate-asdf-file))


(defun generate-lisp-file (class-name &optional (extension ".lisp"))
  (let ((file-name (lisp-name class-name))
        (members (gethash class-name *classes*)))
    (add-lisp-symbol (lisp-name class-name))
    (pushnew (list :file file-name) *lisp-files* :test 'equal)
    (t2f *cl-file-template* (mkstr *lisp-out-dir* file-name extension)
         (list :date (current-date-time-string)
               :file-name (mkstr file-name extension)
               :base-class-name (base-class-name class-name)
               :class-name (lisp-name class-name)
               :f-and-m (iter (for member in (reverse members))
                              (unless (blacklisted member)
                                (collect (generate-lisp-member class-name
                                                               member))))))))


(defun generate-lisp-member (class-name member)
  (let* ((*print-pretty* nil)  ; we want the comments below to stay on one line
         (args (getf member :args))
         (name (getf member :name))
         (type (lisp-type (getf member :type)))
         (c-name (c-name (mkstr class-name "_" name)))
         (lisp-name (lisp-name name))
         (lispefied-c-name (lisp-name c-name))
         (return-type (if (stringp type) type (funcall type :return-type))))
    (add-lisp-symbol lisp-name)
    (list :after (if (stringp type) nil (funcall type :after))
          :args (cl-args args)
          :before (if (stringp type) nil (funcall type :before))
          :c-name c-name
          :lispefied-c-name lispefied-c-name
          :name lisp-name
	  ;; use one of the *-to-string functions instead of format?
          :ogre-name (format nil "~S" name)
          :ogre-type (format nil "~S" (getf member :type))
          :ogre-args (format nil "~S" args)
          :return-arg (if (stringp type) nil (funcall type :return-arg))
          :return-type (if (stringp return-type) return-type (car return-type))
          :return-type-arg (unless (stringp return-type) (cdr return-type)))))


(defun generate-package-file (&optional (file-name "package.lisp"))
  (t2f *cl-package-template* (mkstr *lisp-out-dir* file-name)
       (list :date (current-date-time-string)
             :file-name file-name
             :symbols *lisp-symbols*)))


(defun generate-typedefs-file (&key (file-name "typedefs") (extension ".lisp"))
  (pushnew (list :file file-name) *lisp-files* :test 'equal)
  (t2f *cl-typedefs-template* (mkstr *lisp-out-dir* file-name extension)
       (list :date (current-date-time-string)
             :file-name (mkstr file-name extension)
             :typedefs (iter (for (name type) in-hashtable *typedefs*)
                             (as lisp-name = (lisp-name name))
                             (as lisp-type = (lisp-type type))
                             (collect (list :name lisp-name
                                            :type lisp-type))))))


;;;# Type Handlers

;;;## Type Handler Keywords

;;;### :after
;;;
;;; Code that comes on the line after the call to the cfun.

;;;### :arg-type
;;;
;;; The argument type it should take in the defcfun.

;;;### :before
;;;
;;; Code that precedes the line of the call to the cfun.

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
;;; This is either a string or a cons.  If it is a it just signifies
;;; what the return type of the defcfun should be.  If it is a cons
;;; then its car signifies the return type of the defcfun (generally
;;; :bool or :void) and its cdr signifies that we should also generate
;;; an argument for to the C wrapper function.  This is usually to
;;; pass an array by reference instead of returning a pointer. To take
;;; CL-ARRAY3 as an example again, this is often used for Ogre
;;; functions that return a Vector3.  Instead of returning a pointer
;;; to a Vector3 object we allocate an array that can hold 3
;;; Ogre::Reals, pass that to the C-wrapper which fills it with the x,
;;; y and z values of the Vector3 object and a CFFI translator returns
;;; it as a (vector x y z).  Oh, and the array gets deallocated again
;;; by CFFI.

;;;## Type Handler Functions

(defun cl-array3 (type)
  (case type
    (:after (mkfstr "(vector (mem-aref array 'okra-real 0) "
                            "(mem-aref array 'okra-real 1)~%"
                "            (mem-aref array 'okra-real 2))"))
    (:arg-type "okra-array3")
    (:before "(with-foreign-object (array 'okra-real 3)")
    (:return-arg "array")
    (:return-type '(":void" . "(array3 :pointer)"))
    (otherwise nil)))


(defun cl-array4 (type)
  (case type
    (:after (mkfstr "(vector (mem-aref array 'okra-real 0) "
                            "(mem-aref array 'okra-real 1)~%"
                "            (mem-aref array 'okra-real 2) "
                            "(mem-aref array 'okra-real 3))"))
    (:arg-type "okra-array4")
    (:before "(with-foreign-object (array 'okra-real 4)")
    (:return-arg "array")
    (:return-type '(":void" . "(array4 :pointer)"))
    (otherwise nil)))


(defun cl-array6 (type)
  (case type
    (:after (mkfstr "(vector (mem-aref array 'okra-real 0) "
                            "(mem-aref array 'okra-real 1)~%"
                "            (mem-aref array 'okra-real 2) "
                            "(mem-aref array 'okra-real 3)~%"
                "            (mem-aref array 'okra-real 4) "
                            "(mem-aref array 'okra-real 5))"))
    (:arg-type "okra-array6")
    (:before "(with-foreign-object (array 'okra-real 6)")
    (:return-arg "array")
    (:return-type '(":void" . "(array6 :pointer)"))
    (otherwise nil)))


(defun cl-array16 (type)
  (case type
    (:after (mkfstr "(vector (mem-aref array 'okra-real  0) "
                            "(mem-aref array 'okra-real  1)~%"
                "            (mem-aref array 'okra-real  2) "
                            "(mem-aref array 'okra-real  3)~%"
                "            (mem-aref array 'okra-real  4) "
                            "(mem-aref array 'okra-real  5)~%"
                "            (mem-aref array 'okra-real  6) "
                            "(mem-aref array 'okra-real  7)~%"
                "            (mem-aref array 'okra-real  8) "
                            "(mem-aref array 'okra-real  9)~%"
                "            (mem-aref array 'okra-real 10) "
                            "(mem-aref array 'okra-real 11)~%"
                "            (mem-aref array 'okra-real 12) "
                            "(mem-aref array 'okra-real 13)~%"
                "            (mem-aref array 'okra-real 14) "
                            "(mem-aref array 'okra-real 15))"))
    (:arg-type "okra-array16")
    (:before "(with-foreign-object (array 'okra-real 16)")
    (:return-arg "array")
    (:return-type '(":void" . "(array16 :pointer)"))
    (otherwise nil)))


(defun cl-plane (type)
  (case type
    (:arg-type "okra-array4")
    (:return-type ":pointer")
    (otherwise nil)))


;;; First try to return a CPP vector as a dynamically sized array.  Leaks
;;; like a sieve in combination with c-render-system-list.
;;; (foreign-free rs-list) possibly doesn't work and even if it does it'll
;;; only free the rs-list memory and not that which all of its pointers are
;;; pointing to.
(defun cl-render-system-list (type)
  (case type
    (:after
      (mkfstr "for i from 1 to (parse-integer (mem-aref rs-list :string 0))~%"
              "        collect (mem-aref rs-list :string i)~%"
              "        finally (foreign-free rs-list)"))
    (:before "(loop with rs-list =")  ; should be :before-call like in cpp
    (:return-type ":pointer")
    (otherwise nil)))
