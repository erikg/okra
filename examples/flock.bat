rem flock.bat

PATH=%PATH%;..\lib

wx86cl --load flock.lisp --eval "(in-package :okra)"
