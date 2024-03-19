// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_expected
# error "Feature-test macro for expected missing in <version>"
#elif __cpp_lib_expected != 202211L
# error "Feature-test macro for expected has wrong value in <version>"
#endif

#ifndef __cpp_lib_freestanding_expected
# error "Feature-test macro for freestanding expected missing in <version>"
#elif __cpp_lib_freestanding_expected != 202311L
# error "Feature-test macro for freestanding expected has wrong value in <version>"
#endif

#undef __cpp_lib_expected
#undef __cpp_lib_freestanding_expected
#include <expected>

#ifndef __cpp_lib_expected
# error "Feature-test macro for expected missing in <expected>"
#elif __cpp_lib_expected != 202211L
# error "Feature-test macro for expected has wrong value in <expected>"
#endif

#ifndef __cpp_lib_freestanding_expected
# error "Feature-test macro for freestanding expected missing in <expected>"
#elif __cpp_lib_freestanding_expected != 202311L
# error "Feature-test macro for freestanding expected has wrong value in <expected>"
#endif
