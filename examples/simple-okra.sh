#! /bin/sh

if [ "$OS" = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    wx86cl --load simple-okra.lisp --eval "(in-package :okra)";
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    sbcl --load simple-okra.lisp --eval "(in-package :okra)";
fi
