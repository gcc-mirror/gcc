// { dg-do preprocess { target c++26 } }
// { dg-add-options no_pch }

#include <numeric>

#ifndef __cpp_lib_saturation_arithmetic
# error "Feature test macro for saturation arithmetic is missing in <numeric>"
#elif __cpp_lib_saturation_arithmetic < 202311L
# error "Feature test macro for saturation arithmetic has wrong value in <numeric>"
#endif

#undef __cpp_lib_saturation_arithmetic
#include <version>

#ifndef __cpp_lib_saturation_arithmetic
# error "Feature test macro for saturation arithmetic is missing in <version>"
#elif __cpp_lib_saturation_arithmetic < 202311L
# error "Feature test macro for saturation arithmetic has wrong value in <version>"
#endif
