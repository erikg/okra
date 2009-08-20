#! /bin/sh

if [ $OS = "Windows_NT" ]; then
    PATH=$PATH:../lib;
    ./okra.exe;
else
    LD_LIBRARY_PATH=/usr/local/lib/OGRE:../lib;
    ./okra --no-userinit;
    xset r on;
fi
