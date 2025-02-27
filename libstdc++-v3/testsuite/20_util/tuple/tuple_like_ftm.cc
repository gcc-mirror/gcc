// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <utility>

#if !defined(__cpp_lib_tuple_like)
# error "Feature-test macro for tuple-like is missing"
#elif __cplusplus > 202302L
# if __cpp_lib_tuple_like < 202311L
#  error "Feature-test macro for tuple-like has wrong value"
# endif
#else
# if __cpp_lib_tuple_like < 202207L
#  error "Feature-test macro for tuple-like has wrong value"
# endif
#endif

