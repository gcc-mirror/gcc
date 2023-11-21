// { dg-do compile { target c++26 } }
// { dg-add-options no_pch }

#include <cstring>

#ifndef __cpp_lib_freestanding_cstring
# error "Feature test macro for freestanding <cstring> is missing in <cstring>"
#elif __cpp_lib_freestanding_cstring < 202311L
# error "Feature test macro for freestanding <cstring> has wrong value in <cstring>"
#endif

#undef __cpp_lib_freestanding_cstring
#include <version>

#ifndef __cpp_lib_freestanding_cstring
# error "Feature test macro for freestanding <cstring> is missing in <version>"
#elif __cpp_lib_freestanding_cstring < 202311L
# error "Feature test macro for freestanding <cstring> has wrong value in <version>"
#endif
