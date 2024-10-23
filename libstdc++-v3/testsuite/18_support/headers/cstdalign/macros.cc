// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated" }
// { dg-do preprocess }

#include <cstdalign>

// { dg-error "ISO C.. 2011" "" { target c++98_only } 0 }
// { dg-warning "deprecated" "" { target c++17_only } 0 }
// { dg-error "not a standard header" "" { target c++20 } 0 }

#if __cplusplus >= 201103L
#ifndef __alignas_is_defined
# error "The header <cstdalign> fails to define a macro named  __alignas_is_defined"
#elif __alignas_is_defined != 1
# error "__alignas_is_defined is not defined to 1 in <cstdalign>"
#endif

#ifndef __alignof_is_defined
# error "The header <cstdalign> fails to define a macro named __alignof_is_defined"
#elif __alignof_is_defined != 1
# error "__alignof_is_defined is not defined to 1 in <cstdalign>"
#endif

#ifdef alignas
# error "The header <cstdalign> defines a macro named alignas"
#endif

#ifdef alignof
# error "The header <cstdalign> defines a macro named alignof"
#endif
#endif
