// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_invoke
# error Feature-test macro for invoke is missing in <version>
#elif __cpp_lib_invoke < 201411L
# error Feature-test macro for invoke has the wrong value in <version>
#endif

#if __cplusplus > 202002L
#ifndef __cpp_lib_invoke_r
# error Feature-test macro for invoke_r is missing in <version>
#elif __cpp_lib_invoke_r < 202106L
# error Feature-test macro for invoke_r has the wrong value in <version>
#endif
#elif defined __cpp_lib_invoke_r
# error __cpp_lib_invoke_r is defined in <version> before C++23
#endif
