// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <algorithm>

#ifndef __cpp_lib_freestanding_algorithm
# error "Feature test macro for freestanding fill_n is missing in <algorithm>"
#elif __cpp_lib_freestanding_algorithm < 202311L
# error "Feature test macro for freestanding fill_n has wrong value in <algorithm>"
#endif

#undef __cpp_lib_freestanding_algorithm
#include <version>

#ifndef __cpp_lib_freestanding_algorithm
# error "Feature test macro for freestanding fill_n is missing in <version>"
#elif __cpp_lib_freestanding_algorithm < 202311L
# error "Feature test macro for freestanding fill_n has wrong value in <version>"
#endif
