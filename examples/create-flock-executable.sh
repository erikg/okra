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
    $CL --load create-flock-executable.lisp;
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    $CL --no-userinit --load create-flock-executable.lisp;
fi
