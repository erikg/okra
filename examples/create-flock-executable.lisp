;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; create-flock-executable.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; Make sure the libraries / so's / dll's can be found both when creating
;;;; the executable and when running it!  One of the ways to do this is
;;;; setting LD_LIBRARY_PATH on Unix and PATH on Windows.
;;;;
;;;; This is very SBCL-centric for now.

;;; ASDF

#+sbcl (unless (find-package :asdf)
         (require :asdf))


#+(and sbcl unix) 
  (setf asdf::*central-registry* 
        (list "/usr/local/pub/ekwis/software/Lisp/00-Systems/"))


;;; ABL
;;;
;;; You don't have to use this package but I don't like littering my source
;;; directories with fasls.

(unless (find-package :asdf-binary-locations-system)
  (asdf:oos 'asdf:load-op :asdf-binary-locations))


;;; Flock (make sure to comment out the call to RUN-FLOCK at the toplevel)

(load "flock.lisp")


;;; Saving Executables

(defparameter name #+unix "flock" #+windows "flock.exe")

#+ccl (save-application name :init-file "flock-init.lisp" :prepend-kernel t)

;; From http://sbcl.org/
;;
;;   see: http://www.sbcl.org/manual/Saving-a-Core-Image.html
;; notes: *default-pathname-defaults*, sb-ext:*core-pathname*
#+sbcl (save-lisp-and-die name :executable t :toplevel #'okra::run-flock)
