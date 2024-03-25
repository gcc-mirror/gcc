// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_is_layout_compatible
# error "Feature test macro for is_layout_compatible is missing in <version>"
#elif __cpp_lib_is_layout_compatible < 201907L
# error "Feature test macro for is_layout_compatible has wrong value in <version>"
#endif
