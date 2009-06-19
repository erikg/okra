#! /bin/sh

LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;

sbcl --load simple-okra.lisp --eval "(in-package :okra)";
