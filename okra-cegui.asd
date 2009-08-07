;;;; okra-cegui.asd
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :cl-user)

(defpackage :okra-system
  (:use :cl :asdf))

(in-package :okra-system)

(asdf:defsystem :okra-cegui
  :version "1.6.2.2"
  :components
    ((:module src-cegui
      :serial t
      :components
        ((:file "package") (:file "cffi") (:file "cegui"))))
  :depends-on (:cffi :okra :okra-common))
