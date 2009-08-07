#! /bin/sh

echo NOTE: Use bin/rsync-generated-files if you have rsync installed!;
echo Installing new bindings...;

rm -f ../okra-bindings.asd \
      ../libokra/build/CMakeLists.txt \
      ../libokra/src/*.cpp \
      ../src-bindings/*.fasl \
      ../src-bindings/*.lisp;

cp -p ./generated/okra-bindings.asd ..;
cp -p ./generated/libokra/build/CMakeLists.txt ../libokra/build/;
cp -p ./generated/libokra/src/*.cpp ../libokra/src/;
cp -p ./generated/src-bindings/*.lisp ../src-bindings/;

echo Done.;
