;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; okra-init.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.
;;;;
;;;; Init file used by some CL implementation's executables.


#+ccl (progn (buclet::load-foreign-libraries)
             (okra-bindings::load-foreign-libraries)
             (okra-mygui::load-libmygui)
             (clois-lane::load-foreign-libraries))
