;;;; okra-mygui.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra-system
  (:use :cl :asdf))

(in-package :okra-system)

(asdf:defsystem :okra-mygui
  :version "1.6.4.1"
  :components ((:module src-mygui
                :serial t
                :components ((:file "package")
                             (:file "cffi")
                             (:file "mygui"))))
  :depends-on (:cffi :okra :okra-common))
