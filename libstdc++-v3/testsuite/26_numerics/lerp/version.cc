// { dg-options "-std=gnu++2a" }
// { dg-do preprocess { target c++2a } }
// { dg-require-effective-target hosted }

#include <version>

#ifndef __cpp_lib_interpolate
# error "Feature-test macro for midpoint and lerp missing in <version>"
#elif __cpp_lib_interpolate != 201902L
# error "Feature-test macro for midpoint and lerp has wrong value in <version>"
#endif
