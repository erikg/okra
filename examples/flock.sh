#!/bin/sh

# $OSTYPE isn't exported by my Linux bash shell.
if [ "$OSTYPE" = "" ]; then
    OSTYPE=`uname`;
fi


if [ "$OSTYPE" = "Linux" -o "$OSTYPE" = "linux-gnu" ]; then
    # I have to do an 'export' apparently...
    export LD_LIBRARY_PATH=/lib:/usr/lib:/usr/local/lib/OGRE:../lib;
    if command -v sbcl 1>&2 > /dev/null; then  # Preferring SBCL on Linux.
        CL="sbcl";
    elif command -v lx86cl 1>&2 > /dev/null; then
        CL="lx86cl";
    elif command -v clisp 1>&2 > /dev/null; then
        CL="clisp";
    else
        echo "Could not find SBCL, CCL or CLISP in PATH... aborting.";
        exit 1;
    fi
#elif OSX
#    DYLD_LIBRARY_PATH=../lib:${DYLD_LIBRARY_PATH};
# ...
elif [ "$OSTYPE" = "Windows_NT" -o "$OSTYPE" = "msys" ]; then
    PATH=$PATH:../lib;
    if command -v wx86cl 1>&2 > /dev/null; then  # Preferring CCL on Windows.
        CL="wx86cl";
    elif command -v sbcl 1>&2 > /dev/null; then
        CL="sbcl";
    elif command -v clisp 1>&2 > /dev/null; then
        CL="clisp";
    else
        echo "Could not find SBCL, CCL or CLISP in PATH... aborting.";
        exit 1;
    fi
fi


if [ "$CL" = "sbcl" -o "$CL" = "lx86cl" -o "$CL" = "wx86cl" ]; then
    $CL --load flock.lisp --eval "(in-package :okra)";
elif [ "$CL" = "clisp" ]; then
    $CL -ansi -repl -i flock.lisp -x "(in-package :okra)";
fi


if [ "$OSTYPE" = "Linux" -o "$OSTYPE" = "linux-gnu" ]; then
    xset r on;
fi

exit 0;
