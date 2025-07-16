// { dg-do preprocess { target c++26 } }
// { dg-add-options no_pch }

#include <exception>

#ifdef __cpp_lib_constexpr_exceptions
# error "Feature test macro for constexpr_exceptions should not be provided by <exception>"
#endif

