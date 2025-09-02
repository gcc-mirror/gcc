// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <mdspan>

#ifndef __cpp_lib_mdspan
#error "Feature test macro __cpp_lib_mdspan is missing for <mdspan>"
#elif __cplusplus <= 202302L && __cpp_lib_mdspan != 202207L
#error "Feature test macro __cpp_lib_mdspan has the wrong value for C++23"
#elif __cplusplus > 202302L && __cpp_lib_mdspan != 202406L
#error "Feature test macro __cpp_lib_mdspan has the wrong value for C++26"
#endif

#if __cplusplus > 202302L
#ifndef __cpp_lib_aligned_accessor
#error "Feature test macro __cpp_lib_aligned_accessor is missing for <mdspan>"
#elif __cpp_lib_aligned_accessor != 202411L
#error "Feature test macro __cpp_lib_aligned_accessor has the wrong value"
#endif
#endif
