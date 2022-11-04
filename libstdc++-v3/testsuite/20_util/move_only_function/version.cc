// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }
// { dg-require-effective-target hosted }

#include <version>

#ifndef __cpp_lib_move_only_function
# error "Feature-test macro for move_only_function missing in <version>"
#elif __cpp_lib_move_only_function != 202110L
# error "Feature-test macro for move_only_function has wrong value in <version>"
#endif
