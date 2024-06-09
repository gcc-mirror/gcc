// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_variant
#error "Feature test macro for variant is missing in <version>"
#elif __cplusplus == 201703L && __cpp_lib_variant != 202102L
# error "Feature test macro for variant has wrong value for C++17 in <version>"
#elif __cplusplus >= 202002L && __cpp_lib_variant < 202106L
# error "Feature test macro for variant has wrong value for C++20 in <version>"
#endif

#if __cplusplus >= 202302L
#ifndef __cpp_lib_freestanding_variant
# error "Feature test macro for freestanding std::variant is missing in <version>"
#elif __cpp_lib_freestanding_variant < 202311L
# error "Feature test macro for freestanding std::variant has wrong value in <version>"
#endif
#endif

#undef __cpp_lib_variant
#undef __cpp_lib_freestanding_variant
#include <variant>

#if __cplusplus >= 202302L
#ifndef __cpp_lib_freestanding_variant
# error "Feature test macro for freestanding std::variant is missing in <variant>"
#elif __cpp_lib_freestanding_variant < 202311L
# error "Feature test macro for freestanding std::variant has wrong value in <variant>"
#endif
#endif
