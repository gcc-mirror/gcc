// { dg-do compile { target c++17 } }
// { dg-require-effective-target hosted }

#include <version>

#ifndef __cpp_lib_constexpr_string
# error "Feature-test macro for constexpr std::string missing in <version>"
#endif

#if __cplusplus == 201703L
# if __cpp_lib_constexpr_string != 201611L
#  error "Feature-test macro for constexpr std::string has wrong value for C++17 in <version>"
# endif
#endif

#if __cplusplus == 202002L
# if _GLIBCXX_USE_CXX11_ABI
#  if __cpp_lib_constexpr_string != 201907L
#   error "Feature-test macro for constexpr std::string has wrong value for C++20 in <version>"
#  endif
# else // COW strings
#  if __cpp_lib_constexpr_string != 201811L
#   error "Feature-test macro for constexpr std::string has wrong value for C++20 in <version>"
#  endif
# endif
#endif
