;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; common.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

;;;# Information
;;;
;;; This file contains all kinds of assorted functions and macros that I (Erik
;;; Winkels) use, sometimes only during development from the REPL.

(in-package :okra-bindings-generator)


;;;# Functions

(defun blacklisted (member)
  (member member *blacklist* :test 'equal))


(defun c-name (name &optional (no-ogre nil))
  (iter (with prev-char = nil)
        (for char in-string name)
        (cond ;; "X" => "_x"
              ((and (upper-case-p char)
                    (> (length result) 1)  ; don't put a #\_ at the front
                    (not (upper-case-p prev-char)))  ; don't seperate abbrevs
               (collecting #\_ into result)
               (collecting char into result))
              ;; " " => "_"
              ((equal char #\Space) (collecting #\_ into result))
              ;; Otherwise just collect the char.
              (t (collecting char into result)))
        (setf prev-char char)
        (finally (return (mkstr (if no-ogre "" "ogre_")
                                (string-downcase (coerce result 'string)))))))


;; XXX: this is to cut down on warnings during development
(defparameter *c-type-mapping* nil)  ; redefined in cpp-config.lisp

(defun c-type (type &optional (key nil))
  (cond ;; mappings
        ((assoc type *c-type-mapping* :test 'equal)
         (funcall (cdr (assoc type *c-type-mapping* :test 'equal)) key))
        ;; pointers (that didn't need mapping)
        ;((and (> (length type) 1)
        ;      (or (equal (subseq type (- (length type) 1)) "&")
        ;          (equal (subseq type (- (length type) 1)) "*")))
        ; type)
        ;; enums
        ((gethash type *enum-types*)
         ;; add :overloaded-type as well?
         (if (or (equal key :arg-type) (equal key :return-type))
             type
             nil))
        ;; typedefs
        ((gethash type *typedefs*)
         ;; add :overloaded-type as well?
         (if (or (equal key :arg-type) (equal key :return-type))
             (gethash type *typedefs*)
             nil))
        ;; simple argument and return types
        ((or (equal key :arg-type) (equal key :return-type))
         type)
        ;; everything else
        (t nil)))
        ;(t (error "[c-type] Unknown type: ~S~%" type))))


(defun chop-namespace (string &optional (namespace "Ogre::"))
  "Chops the string NAMESPACE (by default \"Ogre::\") from STRING."
  (let ((ns-length (length namespace)))
    (if (and (>= (length string) ns-length)
             (equal (subseq string 0 ns-length) namespace))
        (subseq string ns-length)
        string)))


(defun first-match (regex string)
  (elt (nth-value 1 (cl-ppcre:scan-to-strings regex string)) 0))


(defun initialise-templates ()
  (loop for cons in *template-list*
        do (setf (gethash (car cons) *templates*)
                 (create-template-printer (cdr cons)))))


(defun lisp-name (name)
  (iter (with prev-char = nil)
        (for char in-string name)
        (cond ;; "X" => "-x"
              ((and (upper-case-p char)
                    (> (length result) 1)  ; don't put a #\- at the front
                    (not (upper-case-p prev-char)))  ; don't seperate abbrevs
               (collecting #\- into result)
               (collecting char into result))
              ;; " " => "-"
              ((equal char #\Space) (collecting #\- into result))
              ;; "_" => "-"
              ((equal char #\_)
               (when (> (length result) 1)  ; don't put a #\- at the front
                 (collecting #\- into result)))
              ;; Otherwise just collect the char.
              (t (collecting char into result)))
        (setf prev-char char)
        (finally (return (string-downcase (coerce result 'string))))))


;; XXX: this is to cut down on warnings during development
(defparameter *lisp-type-mapping* nil)  ; redefined in common-lisp-config.lisp

(defun lisp-type (type &optional (key nil))
  (cond ;; mappings
        ((assoc type *lisp-type-mapping* :test 'equal)
         (funcall (cdr (assoc type *lisp-type-mapping* :test 'equal)) key
                  type))
        ;; pointers
        ((and (> (length type) 1)
              (or (equal (subseq type (- (length type) 1)) "&")
                  (equal (subseq type (- (length type) 1)) "*")))
         (cl-pointer key type))
        ;; enums
        ((gethash type *enum-types*)
         ;; add :overloaded-type as well?
         (cond ((or (equal key :arg-type) (equal key :return-type))
                (lisp-name type))
               ((equal key :overloaded-type)
                "'keyword")  ; XXX: not as specific as I'd like
               (t nil)))
        ;; typedefs (potential infinite loop)
        ((gethash type *typedefs*)
         (lisp-type (gethash type *typedefs*) key))
        ;; everything else
        (t (error "[lisp-type] Unknown type: ~S~%" type))))


(defun memberdef-args (md)
  (let ((params (node :|param| md)))
    (unless params
      (return-from memberdef-args "void"))
    (when (and (equal (length params) 1)
               (equal (values-of (first (children (first params)))) '("void")))
      (return-from memberdef-args "void"))
    (iter (for param in params)
        (as declname = (first (children (first (node :|declname| param)))))
        (as type = (normalised-values-of (first (node :|type| param)) t))
        (collecting (cons type declname)))))


(defun memberdef-name (md)
  (first (children (first (node :|name| md)))))


(defun memberdef-type (md)
  (normalised-values-of (first (node :|type| md)) t))


(defun normalised-values-of (element &optional (as-string nil))
  (let ((result (iter (for value in (values-of element))
                      (collecting (normalise-space value)))))
    (if as-string
        (let ((result (subseq (format nil "~{ ~A~}" result) 1)))
          (cond ((and (> (length result) 2)
                      (equal (subseq result (- (length result) 2)) " *"))
                 (mkstr (subseq result 0 (- (length result) 2)) "*"))
                ((and (> (length result) 2)
                      (equal (subseq result (- (length result) 2)) " &"))
                 (mkstr (subseq result 0 (- (length result) 2)) "&"))
                (t result)))
        result)))


(defun ogre-base-class-name (node)
  (chop-namespace
    (first
      (normalised-values-of
        (first (node '(:|compounddef| :|basecompoundref|) node))))))


(defun ogre-class-name (node)
  (chop-namespace
    (first
      (normalised-values-of
        (first (node '(:|compounddef| :|compoundname|) node))))))


(defun overloaded (member)
  (> (length (gethash (getf member :name) *members*))
     1))


(defun template-to-file (template/printer file values)
  (with-open-file (f file :direction :output :if-exists :supersede)
    (fill-and-print-template template/printer values :stream f)))

(defun t2f (template/printer file values)
  (template-to-file template/printer file values))


(defun template-to-string (template/printer values)
  (with-output-to-string (string)
    (fill-and-print-template template/printer values :stream string)))

(defun t2str (template/printer values)
  (template-to-string template/printer values))
