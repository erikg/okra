# CMakeLists.txt
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in the Okra root directory for more info.

set(LIBCEGUI_SOURCE_FILES "../src/cegui.cpp")

add_library(cegui ${LIBCEGUI_SOURCE_FILES})

if (MINGW)
    target_link_libraries(cegui ${CEGUI_LIBRARY}
                           /c/Software/OgreSDK/bin/release/OgreGUIRenderer.dll)
else ()
    target_link_libraries(cegui ${CEGUI_LIBRARY}
                                /usr/local/lib/libCEGUIOgreRenderer.so)
endif ()
