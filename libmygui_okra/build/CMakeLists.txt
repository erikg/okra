# CMakeLists.txt
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in the Okra root directory for more info.

set(LIBMYGUI_SOURCE_FILES "../src/mygui.cpp")

add_library(mygui_okra ${LIBMYGUI_SOURCE_FILES})

target_link_libraries(mygui_okra ${MYGUI_LIBRARIES})
