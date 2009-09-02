#!/bin/sh

if [ "$OS" = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    ./flock.exe;
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    ./flock --no-userinit;
    xset r on;
fi
