// { dg-options "-std=gnu++23" }
// { dg-do preprocess { target c++23 } }

#include <version>

#ifndef __cpp_lib_monadic_optional
# error "Feature test macro for monadic optional is missing in <version>"
#elif __cpp_lib_monadic_optional < 202110L
# error "Feature test macro for monadic optional has wrong value in <version>"
#endif
