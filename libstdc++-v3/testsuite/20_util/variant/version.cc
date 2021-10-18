// { dg-do compile { target c++17 } }

#include <version>

#ifndef __cpp_lib_variant
#error "Feature test macro for variant is missing in <version>"
#elif __cplusplus == 201703L && __cpp_lib_variant != 202102L
# error "Feature test macro for variant has wrong value for C++17 in <version>"
#elif __cplusplus >= 202002L && __cpp_lib_variant < 202106L
# error "Feature test macro for variant has wrong value for C++20 in <version>"
#endif
