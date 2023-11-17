// { dg-do preprocess { target c++23 } }
// { dg-add-options no_pch }

#include <memory>

#ifndef __cpp_lib_out_ptr
# error "Feature test macro for out_ptr is missing in <memory>"
#elif __cpp_lib_out_ptr < 202106L
# error "Feature test macro for out_ptr has wrong value in <memory>"
#endif

#undef __cpp_lib_out_ptr
#include <version>

#ifndef __cpp_lib_out_ptr
# error "Feature test macro for out_ptr is missing in <version>"
#elif __cpp_lib_out_ptr < 202106L
# error "Feature test macro for out_ptr has wrong value in <version>"
#endif
