// { dg-do preprocess { target c++17 } }
// { dg-add-options no_pch }

#include <functional>

#ifndef __cpp_lib_not_fn
# error "Feature test macro for not_fn is missing in <functional>"
#elif __cpp_lib_not_fn < 201603L
# error "Feature test macro __cpp_lib_not_fn has the wrong value for C++17 or later"
#elif __cplusplus > 202302L && __cpp_lib_not_fn < 202306L
# error "Feature test macro __cpp_lib_not_fn has the wrong value for C++26 or later"
#endif
