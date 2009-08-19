#! /bin/sh

LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;

sbcl --no-userinit --load create-executable.lisp;
