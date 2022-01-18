// { dg-options "-std=gnu++23" }
// { dg-do preprocess { target c++23 } }
// { dg-require-effective-target stacktrace }

#include <version>

#ifndef __cpp_lib_stacktrace
# error "Feature-test macro for stacktrace missing in <version>"
#elif __cpp_lib_stacktrace < 202011L
# error "Feature-test macro for stacktrace has wrong value in <version>"
#endif
