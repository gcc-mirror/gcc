// { dg-do compile { target c++17 } }

#include <version>

#ifndef __cpp_lib_optional
# error "Feature test macro for optional is missing in <version>"
#elif __cplusplus == 201703L && __cpp_lib_optional != 201606L
# error "Feature test macro for optional has wrong value for C++17 in <version>"
#elif __cplusplus >= 202002L && __cpp_lib_optional < 202106L
# error "Feature test macro for optional has wrong value for C++20 in <version>"
#endif
