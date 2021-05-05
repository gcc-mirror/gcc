// { dg-do compile { target c++17 } }

#include <version>

#ifndef __cpp_lib_invoke
# error Feature-test macro for invoke is missing in <version>
#elif __cpp_lib_invoke < 201411L
# error Feature-test macro for invoke has the wrong value in <version>
#endif
