// { dg-options "-std=gnu++23" }
// { dg-do preprocess { target c++23 } }
// { dg-require-effective-target hosted }

#include <version>

#ifndef __cpp_lib_to_chars
# error "Feature-test macro for to_chars missing in <version>"
#elif __cpp_lib_to_chars != 201611L
# error "Feature-test macro for to_chars has wrong value in <version>"
#endif

#ifndef __cpp_lib_constexpr_charconv
# error "Feature-test macro for constexpr charconv missing in <version>"
#elif __cpp_lib_constexpr_charconv != 202207L
# error "Feature-test macro for constexpr charconv has wrong value in <version>"
#endif
