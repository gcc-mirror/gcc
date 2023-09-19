// { dg-do compile { target c++17 } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_constexpr_string
# error Feature-test macro for constexpr char_traits is missing in <version>
#elif __cpp_lib_constexpr_string < (__cplusplus == 201703 ? 201611 : 201811)
# error Feature-test macro for constexpr char_traits has the wrong value in <version>
#endif

// We also provide this non-standard macro for P0426R1 and P1032R1.
#ifndef __cpp_lib_constexpr_char_traits
# error Feature-test macro for constexpr char_traits is missing in <version>
#elif __cpp_lib_constexpr_char_traits != (__cplusplus == 201703 ? 201611 : 201811)
# error Feature-test macro for constexpr char_traits has the wrong value in <version>
#endif
