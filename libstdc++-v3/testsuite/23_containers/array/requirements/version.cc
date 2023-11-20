// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <array>

#ifndef __cpp_lib_freestanding_array
# error "Feature test macro for freestanding std::array is missing in <array>"
#elif __cpp_lib_freestanding_array < 202311L
# error "Feature test macro for freestanding std::array has wrong value in <array>"
#endif

#undef __cpp_lib_freestanding_array
#include <version>

#ifndef __cpp_lib_freestanding_array
# error "Feature test macro for freestanding std::array is missing in <version>"
#elif __cpp_lib_freestanding_array < 202311L
# error "Feature test macro for freestanding std::array has wrong value in <version>"
#endif
