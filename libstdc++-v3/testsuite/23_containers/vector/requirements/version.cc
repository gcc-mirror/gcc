// { dg-do preprocess { target c++20 } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_constexpr_vector
# error "Feature test macro for constexpr vector is missing in <version>"
#elif __cpp_lib_constexpr_vector != 201907L
# error "Feature test macro for constexpr vector has wrong value in <version>"
#endif
