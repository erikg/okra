;;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;;;
;;;; macros.lisp
;;;;
;;;; author: Erik Winkels (aerique@xs4all.nl)
;;;;
;;;; See the LICENSE file in the Okra root directory for more info.

(in-package :okra)


;;;# Macros

(defmacro with-2d ((x-var x-max y-var y-max &key (x-min 0) (y-min 0) (step 1))
                   &body body)
  `(loop for ,y-var from ,y-min below ,y-max by ,step
         do (loop for ,x-var from ,x-min below ,x-max by ,step
                  do ,@body)))


(defmacro with-3d ((x-var x-max y-var y-max z-var z-max &key (x-min 0)
                   (y-min 0) (z-min 0) (step 1)) &body body)
  `(loop for ,z-var from ,z-min below ,z-max
         do (loop for ,y-var from ,y-min below ,y-max by ,step
                  do (loop for ,x-var from ,x-min below ,x-max by ,step
                           do ,@body))))
