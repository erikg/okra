;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; bindings-generator.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;; Functions

;; see OgreRenderSystemCapabilities.h (especially because this is untested!)
;; #define CAPS_CATEGORY_SIZE 4
;; #define OGRE_CAPS_BITSHIFT (32 - CAPS_CATEGORY_SIZE)
;; #define OGRE_CAPS_VALUE(cat, val) ((cat << OGRE_CAPS_BITSHIFT) | (1 << val))
(defun enum-caps-value (string)
  (cl-ppcre:register-groups-bind (cat val)
      ("OGRE_CAPS_VALUE\\((\\S+), (\\d+)\\)" string)
    (logior (ash (gethash cat *enums*) 28)
            (ash 1 (parse-number val)))))


(defun enum-value (string)
  (cond ((cl-ppcre:scan *regex-dec* string)
         (parse-number (first-match *regex-dec* string)))
        ((cl-ppcre:scan *regex-hex* string)
         (parse-number (mkstr "#x" (first-match *regex-hex* string))))
        ((cl-ppcre:scan *regex-con* string)
         (let ((constant (first-match *regex-con* string)))
           (cond ((gethash constant *enums*) (gethash constant *enums*))
                 ((equal (subseq constant 0 16) "OGRE_CAPS_VALUE(")
                  (enum-caps-value constant))
                 ;; hardcoding these enum values for now, since Doxygen
                 ;; doesn't put them in the xml
                 ((equal constant "HardwareBuffer::HBU_STATIC") 1)
                 ((equal constant "HardwareBuffer::HBU_DYNAMIC") 2)
                 ((equal constant "HardwareBuffer::HBU_WRITE_ONLY") 4)
                 ((equal constant "HardwareBuffer::HBU_STATIC_WRITE_ONLY") 5)
                 ((equal constant "HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY") 6)
                 ((equal constant "HardwareBuffer::HBU_DYNAMIC_WRITE_ONLY_DISCARDABLE") 14)
                 ((equal constant "TU_AUTOMIPMAP | TU_STATIC_WRITE_ONLY") 3)
                 (t (error "Could not parse enum constant: ~S" constant)))))
        (t (error "Could not parse enum: ~S" string))))


(defun parse-doxygen-enums-file (file)
  (iter (for enum in (node '(:|compounddef| :|sectiondef| :|memberdef|)
                           (parse-xml-file file)))
        (unless (and (equal (attribute :|kind| enum) "enum")
                     (equal (attribute :|prot| enum) "public"))
          (next-iteration))
        (as type-name = (value-of (first (node :|name| enum))))
        (iter (with previous-value = 0)
              (for value in (node :|enumvalue| enum))
              (as initializer = (value-of (first (node :|initializer| value))))
              (as name = (value-of (first (node :|name| value))))
              (push name (gethash type-name *enum-types*))
              (if (equal initializer "")
                  (progn (setf (gethash name *enums*) previous-value)
                         (incf previous-value))
                  (setf (gethash name *enums*) (enum-value initializer))))))


(defun parse-doxygen-typedefs-file (file)
  (iter (for typedef in (node '(:|compounddef| :|sectiondef| :|memberdef|)
                           (parse-xml-file file)))
        (unless (equal (attribute :|kind| typedef) "typedef")
          (next-iteration))
        (as name = (value-of (first (node :|name| typedef))))
        (as type = (value-of (first (node :|type| typedef))))
        ;(format t "Considering: ~S => ~S...~%" name type)
        ;; only very basic types are handled
        (when (or (and (not (equal type "UTFString"))
                       (cl-ppcre:scan *regex-caps* type))
                  (cl-ppcre:scan *regex-crap* type)
                  (cl-ppcre:scan *regex-stl* type)
                  (string-equal "real" name))
          (next-iteration))
        (setf (gethash name *typedefs*) type)))


(defun parse-doxygen-files (&optional (files *doxygen-files*))
  (clrhash *typedefs*)
  (format t "Parsing Doxygen files for typedefs...~%")
  (iter (for file in *doxygen-typedefs-files*)
        (parse-doxygen-typedefs-file file))
  (clrhash *enum-types*)
  (clrhash *enums*)
  (format t "Parsing Doxygen files for enums...~%")
  (iter (for file in *doxygen-enums-files*)
        (parse-doxygen-enums-file file))
  (clrhash *base-classes*)
  (clrhash *classes*)
  (iter (for file in files)
        (as xml = (parse-xml-file file))
        (as base-class-name = (ogre-base-class-name xml))
        (as class-name = (ogre-class-name xml))
        (format t "Parsing Doxygen file for ~A class...~%" class-name)
        (setf (gethash class-name *base-classes*) base-class-name)
        (iter (for node in (node '(:|compounddef| :|sectiondef|) xml))
              ;; we're only interested in public methods
              ;(unless (or (equal (attribute :|kind| node) "public-static-func")
              ;            (equal (attribute :|kind| node) "public-func"))
              (unless (equal (attribute :|kind| node) "public-func")
                (next-iteration))
              (iter (for memberdef in (node :|memberdef| node))
                    (as name = (memberdef-name memberdef))
                    (when (or ;; skip constructors
                              (equal name class-name)
                              ;; skip deconstructors
                              (equal (elt name 0) #\~)
                              ;; skip member names starting with an underscore
                              (equal (elt name 0) #\_))
                      (next-iteration))
                    (as type = (memberdef-type memberdef))
                    (as args = (memberdef-args memberdef))
                    (push (list :name name :type type :args args)
                          (gethash class-name *classes*))))))


;;; Main Program

(defun generate-bindings (&optional (verbose nil))
  (setf *verbose* verbose)
  (parse-doxygen-files)
  (generate-cpp-bindings)
  (generate-lisp-bindings))


;; for development
(generate-bindings t)
