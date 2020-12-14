# message(STATUS "*top config-ix* CMAKE_SYSTEM : ${CMAKE_SYSTEM}")

include(CheckIncludeFile)
include(CheckIncludeFileCXX)
#include(CheckLibraryExists)
#include(CheckSymbolExists)
include(CheckFunctionExists)
#include(CheckCXXSourceCompiles)
#include(TestBigEndian)
include(CheckCCompilerFlag)
include(CheckCXXCompilerFlag)

# Flags

check_cxx_compiler_flag(-stdlib=libc++    LIBCODY_CXX_HAS_STDLIB_FLAG)

check_cxx_compiler_flag(-fno-enforce-eh-specs LIBCODY_HAS_NOENFORCE)
check_cxx_compiler_flag(-fno-stack-protector LIBCODY_HAS_NOSTACKPROT)
check_cxx_compiler_flag(-fno-threadsafe-statics LIBCODY_HAS_NOTHREADSAFESTATICS)

check_cxx_compiler_flag(-Wno-gnu-zero-variadic-macro-arguments LIBCODY_CXX_W_GZVMA)

# Address github issue #10
if (NOT CODY_WITHEXCEPTIONS)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -fno-rtti")
  if (LIBCODY_HAS_NOENFORCE)
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-enforce-eh-specs")
  endif()
endif()

if (LIBCODY_HAS_NOSTACKPROT)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-stack-protector")
endif()
if (LIBCODY_HAS_NOTHREADSAFESTATICS)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-threadsafe-statics")
endif()

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -W -Wall -Woverloaded-virtual -Wshadow")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-invalid-offsetof -Wno-unused-variable")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-missing-field-initializers")
if (LIBCODY_CXX_W_GZVMA)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wno-gnu-zero-variadic-macro-arguments")
endif ()
