#! /bin/sh

if [ $OS = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    wx86cl --load create-executable.lisp;
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    sbcl --no-userinit --load create-executable.lisp;
fi
