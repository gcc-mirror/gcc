// { dg-do preprocess { target c++23 } }

#include <format>

#ifndef __cpp_lib_format_ranges
# error "Feature-test macro __cpp_lib_format_ranges missing in <format>"
#elif __cpp_lib_format_ranges != 202207L
# error "Feature-test macro __cpp_lib_format_ranges has wrong value in <format>"
#endif
