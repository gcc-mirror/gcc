// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated -fno-operator-names" }
// { dg-do preprocess }
// { dg-bogus "deprecated" "C++17 deprecated <ciso646> but not <iso646.h>" }

#include <iso646.h>

#ifdef and
# error "The header <iso646.h> defines a macro named and"
#endif

#ifdef and_eq
# error "The header <iso646.h> defines a macro named and_eq"
#endif

#ifdef bitand
# error "The header <iso646.h> defines a macro named bitand"
#endif

#ifdef bitor
# error "The header <iso646.h> defines a macro named bitor"
#endif

#ifdef compl
# error "The header <iso646.h> defines a macro named compl"
#endif

#ifdef not
# error "The header <iso646.h> defines a macro named not"
#endif

#ifdef not_eq
# error "The header <iso646.h> defines a macro named not_eq"
#endif

#ifdef or
# error "The header <iso646.h> defines a macro named or"
#endif

#ifdef or_eq
# error "The header <iso646.h> defines a macro named or_eq"
#endif

#ifdef xor
# error "The header <iso646.h> defines a macro named xor"
#endif

#ifdef xor_eq
# error "The header <iso646.h> defines a macro named xor_eq"
#endif
