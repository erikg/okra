#! /bin/sh
#
# rsync version of install-generated-bindings.sh which doesn't overwrite
# files that have not been changed so that make and the CL implementation
# only have to recompile the updated files.

echo rsyncing new bindings...;

rsync --size-only ./generated/okra-bindings.asd ..;
rsync --size-only ./generated/libokra/build/CMakeLists.txt ../libokra/build/;
rsync --size-only ./generated/libokra/src/*.cpp ../libokra/src/;
rsync --size-only ./generated/src-bindings/*.lisp ../src-bindings/;

echo Done.;
