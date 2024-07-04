// { dg-do preprocess { target c++26 } }
// { dg-require-effective-target hosted }
// { dg-add-options no_pch }

#include <version>

#ifndef __cpp_lib_fstream_native_handle
# error "Feature-test macro for fstream_native_handle missing in <version>"
#elif __cpp_lib_fstream_native_handle != 202306L
# error "Feature-test macro for fstream_native_handle has wrong value in <version>"
#endif
