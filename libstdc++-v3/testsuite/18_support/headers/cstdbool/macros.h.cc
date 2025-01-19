// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated" }
// { dg-do preprocess { target c++11 } }
// { dg-bogus "deprecated" "C++17 deprecated <cstdbool> but not <stdbool.h>" }

#include <stdbool.h>

#ifndef __bool_true_false_are_defined
# error "The header <stdbool.h> fails to define a macro named __bool_true_false_are_defined"
#endif

#ifdef bool
# error "The header <stdbool.h> defines a macro named bool"
#endif

#ifdef true
# error "The header <stdbool.h> defines a macro named true"
#endif

#ifdef false
# error "The header <stdbool.h> defines a macro named false"
#endif
