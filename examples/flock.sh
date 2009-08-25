#! /bin/sh

if [ "$OS" = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    wx86cl --load flock.lisp --eval "(in-package :okra)";
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    sbcl --load flock.lisp --eval "(in-package :okra)";
    xset r on;
fi
