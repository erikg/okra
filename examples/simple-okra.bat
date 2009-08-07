rem simple-okra.bat

PATH=%PATH%;..\lib

wx86cl --load simple-okra.lisp --eval "(in-package :okra)"
