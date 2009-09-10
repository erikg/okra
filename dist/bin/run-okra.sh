#!/bin/sh

if [ "$OS" = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    ./bin/okra;
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    ./bin/okra;
    xset r on;
fi
