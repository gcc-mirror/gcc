// { dg-do preprocess { target c++26 } }
// { dg-add-options no_pch }

#include <stdexcept>

#ifndef __cpp_lib_constexpr_exceptions
# error "Feature test macro for constexpr_exceptions is missing in <stdexcept>"
#elif __cpp_lib_constexpr_exceptions < 202502L
# error "Feature test macro for constexpr_exceptions has wrong value in <stdexcept>"
#endif
