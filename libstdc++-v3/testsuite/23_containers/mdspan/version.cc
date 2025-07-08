// { dg-do compile { target c++23 } }
#include <mdspan>

#ifndef __cpp_lib_mdspan
#error "Feature test macro __cpp_lib_mdspan is missing for <mdspan>"
#if __cpp_lib_mdspan < 202207
#error "Feature test macro __cpp_lib_mdspan has the wrong value"
#endif
#endif
