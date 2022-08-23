// { dg-options "-std=gnu++23" }
// { dg-do preprocess { target c++23 } }

#include <version>

#ifndef __cpp_lib_stdatomic_h
# error "Feature test macro for stdatomic.h is missing in <version>"
#elif __cpp_lib_stdatomic_h != 202011L
# error "Feature test macro for stdatomic.h has wrong value in <version>"
#endif
