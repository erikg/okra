# CMakeLists.txt
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in the Okra root directory for more info.

set(LIBCEGUI_SOURCE_FILES "../src/cegui.cpp")

add_library(cegui ${LIBCEGUI_SOURCE_FILES})

if (MINGW)
    target_link_libraries(cegui ${CEGUI_LIBRARY} ${OGRE_CEGUIRenderer_LIBRARY})
else ()
#    target_link_libraries(cegui ${CEGUI_LIBRARY}
#                                /usr/local/lib/libCEGUIOgreRenderer.so)
    target_link_libraries(cegui ${CEGUI_LIBRARY} ${OGRE_CEGUIRenderer_LIBRARY})
endif ()
