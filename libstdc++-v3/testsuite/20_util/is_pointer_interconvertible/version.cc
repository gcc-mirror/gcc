// { dg-do compile { target c++20 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_is_pointer_interconvertible
# error "Feature test macro for is_pointer_interconvertible is missing in <version>"
#elif __cpp_lib_is_pointer_interconvertible < 201907L
# error "Feature test macro for is_pointer_interconvertible has wrong value in <version>"
#endif
