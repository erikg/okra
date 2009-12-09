#!/bin/sh

# Preferring SBCL.
if command -v sbcl > /dev/null; then
    CL="sbcl";
elif command -v wx86cl > /dev/null; then
    CL="wx86cl";
else
    echo "Could not find SBCL or CCL in PATH... aborting.";
    exit 1;
fi

# XXX: this doesn't quite work on my Linux machine yet
if [ "$OS" = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    $CL --load mygui.lisp --eval "(in-package :okra)";
else
    DYLD_LIBRARY_PATH=../lib:${DYLD_LIBRARY_PATH};  # for OS X
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    $CL --load mygui.lisp --eval "(in-package :okra)";
    xset r on;
fi
