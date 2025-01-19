// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated" }
// { dg-do preprocess { target c++11 } }
// { dg-bogus "deprecated" "C++17 deprecated <cstdalign> but not <stdalign.h>" }

#include <stdalign.h>

#ifndef __alignas_is_defined
# error "The header <stdalign.h> fails to define a macro named  __alignas_is_defined"
#elif __alignas_is_defined != 1
# error "__alignas_is_defined is not defined to 1 in <stdalign.h>"
#endif

#ifndef __alignof_is_defined
# error "The header <stdalign.h> fails to define a macro named __alignof_is_defined"
#elif __alignof_is_defined != 1
# error "__alignof_is_defined is not defined to 1 in <stdalign.h>"
#endif

#ifdef alignas
# error "The header <stdalign.h> defines a macro named alignas"
#endif

#ifdef alignof
# error "The header <stdalign.h> defines a macro named alignof"
#endif
