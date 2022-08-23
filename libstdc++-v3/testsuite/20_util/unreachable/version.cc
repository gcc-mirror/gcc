// { dg-options "-std=gnu++23" }
// { dg-do preprocess { target c++23 } }

#include <version>

#ifndef __cpp_lib_unreachable
# error "Feature-test macro for unreachable missing in <version>"
#elif __cpp_lib_unreachable != 202202L
# error "Feature-test macro for unreachable has wrong value in <version>"
#endif
