// { dg-options "-D_GLIBCXX_USE_DEPRECATED=1 -Wno-deprecated" }
// { dg-do preprocess { target c++11 } }

#include <cstdalign>

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
