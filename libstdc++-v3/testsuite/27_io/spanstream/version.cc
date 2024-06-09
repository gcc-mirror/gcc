// { dg-do compile { target c++23 } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_spanstream
# error "Feature-test macro for spanstream missing in <version>"
#elif __cpp_lib_spanstream != 202106L
# error "Feature-test macro for spanstream has wrong value in <version>"
#endif
