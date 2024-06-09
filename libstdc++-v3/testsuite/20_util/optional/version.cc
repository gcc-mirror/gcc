// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_optional
# error "Feature test macro for optional is missing in <version>"
#elif __cplusplus == 201703L && __cpp_lib_optional != 201606L
# error "Feature test macro for optional has wrong value for C++17 in <version>"
#elif __cplusplus == 202002L && __cpp_lib_optional != 202106L
# error "Feature test macro for optional has wrong value for C++20 in <version>"
#elif __cplusplus > 202002L && __cpp_lib_optional != 202110L
# error "Feature test macro for optional has wrong value for C++23 in <version>"
#endif

#if __cplusplus >= 202302L
#ifndef __cpp_lib_freestanding_optional
# error "Feature test macro for freestanding std::optional is missing in <version>"
#elif __cpp_lib_freestanding_optional < 202311L
# error "Feature test macro for freestanding std::optional has wrong value in <version>"
#endif
#endif

#undef __cpp_lib_optional
#undef __cpp_lib_freestanding_optional
#include <optional>

#if __cplusplus >= 202302L
#ifndef __cpp_lib_freestanding_optional
# error "Feature test macro for freestanding std::optional is missing in <optional>"
#elif __cpp_lib_freestanding_optional < 202311L
# error "Feature test macro for freestanding std::optional has wrong value in <optional>"
#endif
#endif
