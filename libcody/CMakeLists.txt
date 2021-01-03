# Top Level CMake file for libcody.

cmake_minimum_required(VERSION 3.4.3)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "No build type selected, default to MinSizeRel")
  set(CMAKE_BUILD_TYPE MinSizeRel)
  set(LIBCODY_ENABLE_ASSERTIONS 1)
endif()
string(TOUPPER "${CMAKE_BUILD_TYPE}" uppercase_CMAKE_BUILD_TYPE)

set(cmake_3_2_USES_TERMINAL USES_TERMINAL)

if( CMAKE_SOURCE_DIR STREQUAL CMAKE_BINARY_DIR AND NOT MSVC_IDE)
  message(FATAL_ERROR "In-source builds are not allowed. ")
endif()

# message(STATUS "SRC ${CMAKE_SOURCE_DIR} CSRC : ${CMAKE_CURRENT_SOURCE_DIR} ")

# Add path for custom modules
set(CMAKE_MODULE_PATH
    ${CMAKE_MODULE_PATH}
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake"
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/modules")

# If we are building stand-alone, set up the names and versions.
if(CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR
   OR LIBCODY_STANDALONE_BUILD)

  project(libcody CXX)

  set(PACKAGE_NAME codylib)
  set(LIBCODY_VERSION_MAJOR 0)
  set(LIBCODY_VERSION_MINOR 0)
  set(LIBCODY_VERSION_PATCH 1)
  set(LIBCODY_VERSION_SUFFIX git)
  set(LIBCODY_VERSION "${LIBCODY_VERSION_MAJOR}.${LIBCODY_VERSION_MINOR}.${LIBCODY_VERSION_PATCH}")
  set(PACKAGE_VERSION "${LIBCODY_VERSION}-${LIBCODY_VERSION_SUFFIX}")
  set(PACKAGE_STRING "${PACKAGE_NAME} ${PACKAGE_VERSION}")
  set(PACKAGE_URL "https://github.com/urnathan/libcody")
  set(PACKAGE_BUGREPORT "https://github.com/urnathan/libcody/issues")

  set (GIT_REV "git" "-C" "${CMAKE_CURRENT_SOURCE_DIR}" "rev-parse" "--short=12" "HEAD")
  execute_process(
      COMMAND ${GIT_REV}
      RESULT_VARIABLE HAD_ERROR
      OUTPUT_VARIABLE CODY_REVISION
    )
  if (NOT HAD_ERROR)
    string(REGEX REPLACE "\n$" "" CODY_REVISION "${CODY_REVISION}")
    set (GIT_CHANGES "git" "-C" "${CMAKE_CURRENT_SOURCE_DIR}" "diff-index" "--quiet" "HEAD" "--")
    execute_process(
      COMMAND ${GIT_CHANGES}
      RESULT_VARIABLE MOD_ERROR
      OUTPUT_VARIABLE MOD_OUTPUT
    )
    if (MOD_ERROR)
      set (CODY_REVISION "${CODY_REVISION}-modified")
    endif ()
  else()
    set(CODY_REVISION, "unknown")
  endif ()
  set(LIBCODY_STANDALONE YES)
else()
  set(LIBCODY_STANDALONE NO)
endif()

# We are using C++11
set (CMAKE_CXX_STANDARD 11)

message(STATUS "git revision ${CODY_REVISION} ")
option(CODY_CHECKING "Enable checking" ON)
# Address github issue #10
option(CODY_WITHEXCEPTIONS "Enable exceptions" OFF)

if (LIBCODY_STANDALONE)
  include(CTest)
endif()

include(libcody-config-ix)

add_definitions(
 -DPACKAGE_URL="${PACKAGE_URL}"
 -DBUGURL="${PACKAGE_BUGREPORT}"
 -DSRCDIR="${CMAKE_CURRENT_SOURCE_DIR}"
 -DPACKAGE_NAME="${PACKAGE_NAME}"
 -DPACKAGE_STRING="${PACKAGE_STRING}"
 -DPACKAGE_VERSION="${LIBCODY_VERSION}"
 -DREVISION="${CODY_REVISION}"
 )
if (CODY_CHECKING)
  add_definitions(-DNMS_CHECKING=1)
else()
  add_definitions(-DNMS_CHECKING=0)
endif()

set(LIBCODY_SOURCES
  buffer.cc
  client.cc
  fatal.cc
  netclient.cc
  netserver.cc
  resolver.cc
  packet.cc
  server.cc)

if(LIBCODY_STANDALONE)
  add_library(cody STATIC ${LIBCODY_SOURCES})
else()
  message(STATUS "Configured for in-tree build of libcody as LLVMcody")
  add_llvm_component_library(LLVMcody ${LIBCODY_SOURCES})
endif()

if (LIBCODY_STANDALONE)

  set_target_properties(cody PROPERTIES PUBLIC_HEADER "cody.hh")
  install(TARGETS cody 
        LIBRARY DESTINATION lib
        PUBLIC_HEADER DESTINATION include
  )
endif()
