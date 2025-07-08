// { dg-do compile { target c++26 } }
// { dg-require-effective-target hosted }

// N5008 17.3.2 Header <version> synopsis [version.syn]

#include <version>

#ifndef __cpp_lib_smart_ptr_owner_equality
# error "Feature-test macro for smart ptr owner equality missing in <version>"
#elif __cpp_lib_smart_ptr_owner_equality != 202306L
# error "Feature-test macro for smart ptr owner equality has wrong value in <version>"
#endif

