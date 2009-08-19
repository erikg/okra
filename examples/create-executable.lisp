;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; create-executable.lisp
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


#+sbcl (setf asdf::*central-registry*
             (list "/usr/local/pub/ekwis/software/Lisp/00-Systems/"))


;;; ABL
;;;
;;; You don't have to use this package but I don't like littering my source
;;; directories with fasls.

(unless (find-package :asdf-binary-locations-system)
  (asdf:oos 'asdf:load-op :asdf-binary-locations))


;;; Packages

(asdf:oos 'asdf:load-op :buclet)
(asdf:oos 'asdf:load-op :clois-lane)
(asdf:oos 'asdf:load-op :okra)


;;; From http://sbcl.org/
;;;
;;;   see: http://www.sbcl.org/manual/Saving-a-Core-Image.html
;;; notes: *default-pathname-defaults*, sb-ext:*core-pathname*

#+(and ccl windows) (save-application "okra.exe"
                                      :init-file "okra-init.lisp"
                                      :prepend-kernel t)

#+sbcl (save-lisp-and-die "okra" :executable t)
