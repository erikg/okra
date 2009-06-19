;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; lxml-util.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

;;;# Information
;;;
;;; Functions for navigating and retrieving from XML parsed into LXML
;;; form.  This is the default format for S-XML (and maybe XMLS iirc).

(in-package :okra-bindings-generator)


;;;# Functions

(defun attributes? (element)
  "ELEMENT must be a valid LXML element.  Results are undefined if it isn't."
  (when (listp element)
    (listp (car element))))


(defun attributes (element)
  "ELEMENT must be a valid LXML element.  Results are undefined if it isn't."
  (when (attributes? element)
    (cdr (car element))))


(defun attribute (attribute element)
  "ELEMENT must be a valid LXML element.  Results are undefined if it isn't.
  Returns the value of ATTRIBUTE of ELEMENT."
  (when (attributes? element)
    (getf (attributes element) attribute)))


(defun children? (element)
  "ELEMENT must be a valid LXML element.  Results are undefined if it isn't."
  (when (listp element)
    (> (length element) 1)))


(defun children (element)
  "ELEMENT must be a valid LXML element.  Results are undefined if it isn't."
  (when (children? element)
    (cdr element)))


(defun name (element)
  "ELEMENT must be a valid LXML element.  Results are undefined if it isn't."
  (cond ((attributes? element) (car (car element)))
        ((listp element) (car element))
        (t element)))


(defun normalise-space (string)
  "Removes the leading and trailing whitespace from a string.  The official
  XPath function also replaces multiple whitespaces within the string with
  one space, but this function does not do that yet."
  (let ((vector
         (nth-value 1 (cl-ppcre:scan-to-strings "^\\s*(.*?)\\s*$" string))))
    (if vector
        (elt vector 0)
        string)))


(defun normalize-space (string)
  "Calls NORMALISE-SPACE."
  (normalise-space string))


(defun value-of (element)
  "Returns the text-nodes of ELEMENT concatenated as a string."
  (iter (with result = "")
        (for child in (children element))
        (cond ((stringp child)
               (setf result (concatenate 'string result child)))
              ((listp child)
               (setf result (concatenate 'string result (value-of child))))
              (t (format *error-output*
                         "[VALUE-OF] Unexpected child: ~S~%" child)))
        (finally (return result))))


(defun values-of (element)
  "Returns a list with the contents of all the text-nodes of ELEMENT."
  (iter (for child in (children element))
        (cond ((stringp child) (collecting child))
              ((listp child) (appending (values-of child)))
              (t (format *error-output*
                         "[VALUES-OF] Unexpected child: ~S~%" child)))))


;; Still need to add support for:
;; o :|@attribute| - can only be the last item of a list
;; o :|element[n]| - only the nth element (see XPath)
(defun node (path element)
  "PATH can either be a symbol or a list of symbols.
  ELEMENT must be a valid LXML element.

  XPath Lite: returns a list of elements. (Take note: a LIST of elements!)

  Examples for the following (parsed) XML: (:|root|
                                             (:|element| (:|child| \"foo\"))
                                             (:|element| (:|child| \"bar\")))
    These two examples are equivalent:
    o (NODE :|element| ROOT) => ((:|element| (:|child| \"foo\"))
                                 (:|element| (:|child| \"bar\")))
    o (NODE '(:|element|) ROOT) => ((:|element| (:|child| \"foo\"))
                                    (:|element| (:|child| \"bar\")))

    A real path:
    o (NODE '(:|element| :|child|) ROOT) => ((:|child| \"foo\")
                                             (:|child| \"bar\"))"
  (labels ((node-recurser (path element)
             (if (= (length path) 1)
                 (node (car path) element)
                 (iter (for child in (children element))
                       (with name = (car path))
                       (as child-name = (name child))
                       (when (equal child-name name)
                         (appending (node-recurser (cdr path) child)))))))
    (if (listp path)
        (node-recurser path element)
        (iter (for child in (children element))
              (if (equal path :|*|)
                  (collecting child)
                  (when (equal path (name child))
                    (collecting child)))))))
