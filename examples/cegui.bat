rem cegui.bat

PATH=%PATH%;..\lib

wx86cl --load cegui.lisp --eval "(in-package :okra)"
