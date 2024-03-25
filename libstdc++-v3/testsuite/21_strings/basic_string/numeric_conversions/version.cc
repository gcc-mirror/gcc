// { dg-do compile }
// { dg-add-options no_pch }

#include <version>

#if __cplusplus > 202302L

#ifndef __cpp_lib_to_string
# error "Feature-test macro for std::to_string missing in <string>"
#elif __cpp_lib_to_string != 202306L
# error "Feature-test macro for std::to_string has wrong value in <string>"
#endif

#else

#ifdef __cpp_lib_to_string
# error "__cpp_lib_to_string should not be defined for C++23"
#endif

#endif
