#! /bin/sh

LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;

sbcl --load cegui.lisp --eval "(in-package :okra)";

xset r on;
