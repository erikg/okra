#! /bin/sh

LD_LIBRARY_PATH=/usr/local/lib/OGRE:./lib;

cd dist;
cp ../lib/* ./lib;

sbcl --no-userinit --load ../src/make-dist.lisp;
