;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;;
;;;; common-lisp.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra-bindings-generator)


;;;# Functions

(defun add-lisp-file (name)
  (pushnew (list :file name) *lisp-files* :test 'equal))


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
        (as type = (lisp-type (car arg) :arg-type))
        (collect (list :arg-name name :arg-type type))))


(defun collect-enum-types ()
  (iter (for (enum enums) in-hashtable *enum-types*)
        (as type-name = (lisp-name enum))
        (collect
          (list :type-name type-name
                :enums (iter (for name in (reverse enums))
                             (collect
                               (list :name (lisp-name (string-downcase name))
                                     :type (gethash name *enums*))))))))


(defun collect-generics ()
  (loop for v being the hash-values in *members* using (hash-key k)
        for max = (loop for args in v
                        maximize (cond ((and (stringp (getf args :args))
                                             (equal (getf args :args) "void"))
                                        0)
                                       ((stringp (getf args :args)) 1)
                                       (t (length (getf args :args)))))
        collect (list :name (lisp-name k)
                      :overloaded (> (length v) 1)
                      :args (loop for i from 0 below max
                                  collect (list :arg-name (mkstr "arg" i))))))


(defun collect-ignore-list (members args)
  (loop with used-args = (loop for member in members
                               maximize (length (getf member :args)))
        for i from used-args below (length args)
        collect (list :ignore-arg (mkstr "arg" i))))


(defun lispefied-c-name (class-name member)
  (lisp-name (c-fun-name (c-name (mkstr class-name "_" (getf member :name)))
                         member)))


(defun collect-overloaded-args (member)
  (let ((args (getf member :args))
        (name (getf member :name)))
    (if (equal args "void")
        ;; Bit of a hack but easiest to implement: we set all overloaded types
        ;; to nil for a member with no arguments.
        (loop with max = (loop for arglist in (gethash name *members*)
                               for a = (getf arglist :args)
                               maximize (cond ((and (stringp a)
                                                    (equal a "void"))
                                               0)
                                              ((stringp a) 1)
                                              (t (length a))))
              ;; This should use "below" instead of "to", aight?
              for i from 0 below max
              collect (list :arg-name (mkstr "arg" i)
                            :arg-type "'null"))
        (loop for arg in (getf member :args)
              for i from 0
              collect (list :arg-name (mkstr "arg" i)
                            :arg-type (lisp-type (car arg)
                                                 :overloaded-type))))))


(defun collect-overloaded-members (class-name members)
  (loop for member in members
        for type = (getf member :type)
        collect (list :after (lisp-type type :after)
                      :args (collect-overloaded-args member)
                      :before (lisp-type type :before)
                      :lispefied-c-name (lispefied-c-name class-name member)
                      :return-arg (lisp-type type :return-arg)
                      ;; see: collect-overloaded-args
                      :void (when (equal (getf member :args) "void") t))))


(defun generate-asdf-file (&key (file-name "okra-bindings") (extension ".asd"))
  (t2f (gethash :cl-asd-template *templates*)
       (mkstr *lisp-out-dir* "../" file-name extension)
       (list :date (current-date-time-string)
             :file-name (mkstr file-name extension)
             :files (reverse *lisp-files*))))


(defun generate-enums-file (&key (file-name "enums") (extension ".lisp"))
  ;(add-lisp-file file-name)
  (t2f (gethash :cl-enums-template *templates*)
       (mkstr *lisp-out-dir* file-name extension)
       (list :date (current-date-time-string)
             :file-name (mkstr file-name extension)
             :enum-types (collect-enum-types))))


(defun generate-generics-file (&key (file-name "generics") (extension ".lisp"))
  ;(add-lisp-file file-name)
  (t2f (gethash :cl-generics-template *templates*)
       (mkstr *lisp-out-dir* file-name extension)
       (list :date (current-date-time-string)
             :file-name (mkstr file-name extension)
             :generics (collect-generics))))


(defun generate-lisp-bindings (&optional (classes *classes*))
  (setf *lisp-files* nil)
  (setf *lisp-symbols* nil)
  (format t "Generating Common Lisp generics file...~%")
  (generate-generics-file)
  (format t "Generating Common Lisp enums file...~%")
  (generate-enums-file)
  (format t "Generating Common Lisp typedefs file...~%")
  (generate-typedefs-file)
  (loop for name being the hash-keys in classes
        do (format t "Generating Common Lisp bindings for ~A class...~%" name)
           (generate-lisp-file name))
  (format t "Generating Common Lisp package file...~%")
  (generate-package-file)
  (format t "Generating Common Lisp ASDF file...~%")
  (generate-asdf-file))


(defun lisp-members (class-name members &optional (overloaded nil))
  (iter (for member in (reverse members))
        (as member-overloaded = (overloaded member))
        (cond ((blacklisted member) (next-iteration))
              ((and member-overloaded (not overloaded)) (next-iteration))
              ((and (not member-overloaded) overloaded) (next-iteration))
              (t (collect (generate-lisp-member class-name member))))))


(defun overloaded-methods (class-name members)
  (iter (for (k v) in-hashtable *members*)
        (as available = (when (> (length v) 1)  ; overloaded
                          (iter (for m in members)
                                (when (blacklisted m)
                                  (next-iteration))
                                (when (equal k (getf m :name))
                                  (leave t)))))
        (when available
          (collect (generate-overloaded-method class-name members k v)))))


(defun generate-lisp-file (class-name &optional (extension ".lisp"))
  (let ((file-name (lisp-name class-name))
        (members (gethash class-name *classes*)))
    (add-lisp-file file-name)
    (add-lisp-symbol (lisp-name class-name))
    (t2f (gethash :cl-file-template *templates*)
         (mkstr *lisp-out-dir* file-name extension)
         (list :date (current-date-time-string)
               :file-name (mkstr file-name extension)
               :base-class-name (base-class-name class-name)
               :class-name (lisp-name class-name)
               :f-and-m (lisp-members class-name members)
               :overloaded-functions (lisp-members class-name members t)
               :overloaded-methods (overloaded-methods class-name members)))))


(defun generate-lisp-member (class-name member)
  (let* ((*print-pretty* nil)  ; we want the comments below to stay on one line
         (args (getf member :args))
         (name (getf member :name))
         (type (getf member :type))
         (c-name (c-name (mkstr class-name "_" name)))
         (lisp-name (lisp-name name))
         (lispefied-c-name (lisp-name c-name))
         (return-type (lisp-type type :return-type)))
    (add-lisp-symbol lisp-name)
    (list :after (lisp-type type :after)
          :args (cl-args args)
          :before (lisp-type type :before)
          :c-name (c-fun-name c-name member)
          :lispefied-c-name lispefied-c-name
          :name lisp-name
          :ogre-name (write-to-string name)
          :ogre-type (write-to-string type)
          :ogre-args (write-to-string args)
          :return-arg (lisp-type type :return-arg)
          :return-type (if (stringp return-type) return-type (car return-type))
          :return-type-arg (unless (stringp return-type) (cdr return-type)))))


;; this isn't very pretty
(defun generate-overloaded-method (class-name members member-name arglist)
  (let ((args (loop for member in arglist
                    for ma = (getf member :args)
                    maximize (cond ((and (stringp ma) (equal ma "void")) 0)
                                   ((stringp ma) 1)
                                   (t (length ma))) into max-args
                    finally (return (loop for i from 0 below max-args
                                          for name = (mkstr "arg" i)
                                          collect (list :arg-name name)))))
        (overloaded (sort (loop for member in members
                                when (and (not (blacklisted member))
                                          (equal (getf member :name)
                                                 member-name))
                                  collect member)
                          #'>
                          :key (lambda (k) (length (getf k :args))))))
    (list :args args
          ;; this is to cut down on the warnings generated when compiling, so
          ;; we can spot the real warnings
          :ignore-list (collect-ignore-list overloaded args)
          :members (collect-overloaded-members class-name overloaded)
          :name (lisp-name member-name))))


(defun generate-package-file (&optional (file-name "package.lisp"))
  (t2f (gethash :cl-package-template *templates*)
       (mkstr *lisp-out-dir* file-name)
       (list :date (current-date-time-string)
             :file-name file-name
             :symbols *lisp-symbols*)))


(defun generate-typedefs-file (&key (file-name "typedefs") (extension ".lisp"))
  ;(add-lisp-file file-name)
  (t2f (gethash :cl-typedefs-template *templates*)
       (mkstr *lisp-out-dir* file-name extension)
       (list :date (current-date-time-string)
             :file-name (mkstr file-name extension)
             :typedefs (iter (for (name type) in-hashtable *typedefs*)
                             (collect
                               (list :name (lisp-name name)
                                     :type (lisp-type type :arg-type)))))))
