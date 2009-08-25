#! /bin/sh

if [ "$OS" = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    wx86cl --load physics-and-input.lisp --eval "(in-package :okra)";
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    sbcl --load physics-and-input.lisp --eval "(in-package :okra)";
    xset r on;
fi
