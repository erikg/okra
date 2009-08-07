;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; cpp.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Functions

(defun arg-code (args)
  (if (equal args "void")
      nil
      (iter (with result = "")
            (for arg in args)
            (as name = (cdr arg))
            (as type = (if (c-type (car arg) :arg-code)
                           (regex-replace-all "ARG"
                                              (c-type (car arg) :arg-code)
                                              name)
                           ""))
            (setf result (mkstr result (if type type "")))
            (finally (if (equal result "")
                         (return nil)
                         (return result))))))


(defun c-arg-name (arg)
  (flet ((convert (target-string start end match-start match-end reg-starts
                   reg-ends)
           (declare (ignore start end  reg-starts reg-ends))
           (let ((match (subseq target-string match-start match-end)))
             (cond ((equal match " ") "")
                   ((equal match "&") "")
                   ((equal match "*") "")
                   ((equal match ":") "")
                   ((equal match "const") "")))))
    (cl-ppcre:regex-replace-all "( |&|\\*|:|const)"
                                (string-downcase (if (consp arg)
                                                     (car arg)
                                                     arg))
                                #'convert)))


;; mergable with call-args?
(defun c-args (args &key (omit-name nil))
  (if (equal args "void")
      nil
      (iter (with result = "")
            (for arg in args)
            (as name = (cdr arg))
            (as type =  (c-type (car arg) :arg-type))
            (setf result (mkstr result ", " type (if omit-name
                                                     ""
                                                     (mkstr " " name))))
            (finally (return result)))))


(defun c-fun-name (c-name member)
  (if (overloaded member)
      (mkstr c-name
             (if (listp (getf member :args))
                 (loop with result = ""
                       for arg in (getf member :args)
                       do (setf result (mkstr result "_" (c-arg-name arg)))
                       finally (return result))
                 (mkstr "_" (getf member :args))))
      c-name))


;; mergable with c-args?
(defun call-args (args)
  (iter (with result = "")
        (for arg in args)
        (for i from 0)
        (as name = (cdr arg))
        (as type =  (c-type (car arg) :arg-code))
        (unless (= i 0)
          (setf result (mkstr result ", ")))
        (setf result (mkstr result (if type "ogre_" "") name))
        (finally (return result))))


(defun generate-c-fun (class-name member)
  (let* ((*print-pretty* nil)  ; for format below
         (args (getf member :args))
         (c-name (c-name (mkstr class-name "_" (getf member :name))))
         (type (getf member :type))
         (return-type (c-type type :return-type))
         (return-type-arg nil))
    (unless (stringp return-type)
      (setf return-type-arg (mkstr (cadr return-type) " " (cddr return-type)))
      (setf return-type (car return-type)))
    (list :name (getf member :name)
          :type (format nil "~S" type)
          :args (format nil "~S" args)
          ;; make sure a const char* is returned (XXX: check for String!)
          :after-call (if (equal return-type "const char*") ".c_str()" nil)
          :arg-code (arg-code args)
          :before-call (c-type type :before-call)
          :c-args (c-args args)
          :c-name (c-fun-name c-name member)
          :call-args (call-args args)
          :class class-name
          :class-name (c-name class-name t)
          :return (if (or (equal return-type "void")
                          ;; see c-render-system-list
                          (c-type type :no-return))
                      nil
                      "return ")
          :return-type return-type
          :return-type-arg return-type-arg
          :post-code (c-type type :post-code))))


(defun generate-cmakelists (&optional (file-name "CMakeLists.txt"))
  (t2f (gethash :cpp-cmakelists-template *templates*)
       (mkstr *cpp-build-dir* file-name)
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
    (t2f (gethash :cpp-file-template *templates*)
         (mkstr *cpp-src-dir* file-name)
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
        (as return-type = (c-type (getf member :type) :return-type))
        (as return-type-arg = (if (stringp return-type)
                                  nil
                                  (cadr return-type)))
        (unless (stringp return-type)
          (setf return-type (car return-type)))
        (collect (list :return-type return-type
                       :name (c-fun-name c-name member)
                       :class class-name
                       :return-type-arg return-type-arg
                       :args (c-args args :omit-name t)))))
