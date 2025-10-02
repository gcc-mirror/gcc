// { dg-do preprocess { target c++26 } }
// { dg-add-options no_pch }

#include <type_traits>

#ifndef __cpp_lib_constant_wrapper
#error "Feature test macro __cpp_lib_constant_wrapper is missing for <type_traits>"
#if __cpp_lib_constant_wrapper < 202506L
#error "Feature test macro __cpp_lib_constant_wrapper has the wrong value"
#endif
#endif
