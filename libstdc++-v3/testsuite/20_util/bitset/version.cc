// { dg-do preprocess { target c++23 } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_constexpr_bitset
# error "Feature-test macro for constexpr bitset missing in <version>"
#elif __cpp_lib_constexpr_bitset != 202202L
# error "Feature-test macro for constexpr bitset has wrong value in <version>"
#endif
