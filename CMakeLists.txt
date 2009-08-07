# CMakeLists.txt
#
# author: Erik Winkels (aerique@xs4all.nl)
#
# See the LICENSE file in the Okra root directory for more info.

cmake_minimum_required(VERSION 2.6)

project(libokra)

option(BUILD_SHARED_LIBS "build shared libraries" ON)
option(BUILD_CEGUI "build libcegui" ON)

add_definitions(-g -O0 -Wall)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake-modules")

find_package(OGRE REQUIRED)
message("-- Found: ${OGRE_INCLUDE_DIR}")
include_directories(${OGRE_INCLUDE_DIR})
if (BUILD_CEGUI)
    find_package(CEGUI REQUIRED)
    message("-- Found CEGUI: ${CEGUI_LIBRARY}")
    message("-- Found: ${CEGUI_INCLUDE_DIR}")
    include_directories(${CEGUI_INCLUDE_DIR})
    # For the Win32 OgreSDK.
    include_directories(${OGRE_INCLUDE_DIR}/../samples/include)
endif ()

# Set some directories / variables.
set(CMAKE_INSTALL_PREFIX ${CMAKE_SOURCE_DIR})
set(LIBRARY_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/lib)

add_subdirectory(${CMAKE_SOURCE_DIR}/libokra/build)
if (BUILD_CEGUI)
   add_subdirectory(${CMAKE_SOURCE_DIR}/libcegui/build)
endif ()
