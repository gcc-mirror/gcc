// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_forward_like
# error "Feature-test macro for forward_like missing in <version>"
#elif __cpp_lib_forward_like != 202207L
# error "Feature-test macro for forward_like has wrong value in <version>"
#endif
