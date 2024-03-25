// { dg-do compile { target c++23 } }
// { dg-add-options no_pch }

#include <string_view>

#ifndef __cpp_lib_freestanding_string_view
# error "Feature test macro for freestanding std::string_view is missing in <string_view>"
#elif __cpp_lib_freestanding_string_view < 202311L
# error "Feature test macro for freestanding std::string_view has wrong value in <string_view>"
#endif

#undef __cpp_lib_freestanding_string_view
#include <version>

#ifndef __cpp_lib_freestanding_string_view
# error "Feature test macro for freestanding std::string_view is missing in <version>"
#elif __cpp_lib_freestanding_string_view < 202311L
# error "Feature test macro for freestanding std::string_view has wrong value in <version>"
#endif
