// { dg-do compile { target c++17 } }
// { dg-add-options no_pch }

#include <memory>

#ifndef __cpp_lib_raw_memory_algorithms
# error "Feature-test macro for raw memory algorithms missing"
#elif __cplusplus > 202302L
# if __cpp_lib_raw_memory_algorithms < 202411L
#  error "Feature-test macro for raw memory algorithms has wrong value"
# endif
#elif __cpp_lib_raw_memory_algorithms < 201606L
# error "Feature-test macro for raw memory algorithms has wrong value"
#endif
