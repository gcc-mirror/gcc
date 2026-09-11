// { dg-do preprocess { target c++26 } }
// { dg-add-options no_pch }

#include <inplace_vector>

#ifndef __cpp_lib_inplace_vector
# error "Feature-test macro for inplace_vector missing in <inplace_vector>"
#elif __cpp_lib_inplace_vector != 202603L
# error "Feature-test macro for inplace_vector has wrong value in <inplace_vector>"
#endif

#ifndef __cpp_lib_constexpr_inplace_vector
# error "Feature-test macro for constexpr_inplace_vector missing in <constexpr_inplace_vector>"
#elif __cpp_lib_constexpr_inplace_vector != 202502LL
# error "Feature-test macro for constexpr_inplace_vector has wrong value in <constexpr_inplace_vector>"
#endif

#undef __cpp_lib_inplace_vector

#include <version>

#ifndef __cpp_lib_inplace_vector
# error "Feature-test macro for inplace_vector missing in <version>"
#elif __cpp_lib_inplace_vector != 202603L
# error "Feature-test macro for inplace_vector has wrong value in <version>"
#endif

#ifndef __cpp_lib_constexpr_inplace_vector
# error "Feature-test macro for constexpr_inplace_vector missing in <constexpr_inplace_vector>"
#elif __cpp_lib_constexpr_inplace_vector != 202502LL
# error "Feature-test macro for constexpr_inplace_vector has wrong value in <constexpr_inplace_vector>"
#endif
