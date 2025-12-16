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

#if __cplusplus > 202302L && _GLIBCXX_USE_CXX11_ABI
# ifndef __cpp_lib_constexpr_exceptions
#  error "Feature test macro for constexpr_exceptions is missing in <version>"
# elif __cpp_lib_constexpr_exceptions < 202502L
#  error "Feature test macro for constexpr_exceptions has wrong value in <version>"
# endif
#endif

#undef __cpp_lib_expected
#undef __cpp_lib_freestanding_expected
#undef __cpp_lib_constexpr_exceptions
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

#if __cplusplus > 202302L && _GLIBCXX_USE_CXX11_ABI
# ifndef __cpp_lib_constexpr_exceptions
#  error "Feature test macro for constexpr_exceptions is missing in <expected>"
# elif __cpp_lib_constexpr_exceptions < 202502L
#  error "Feature test macro for constexpr_exceptions has wrong value in <expected>"
# endif
#endif
