// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated -fno-operator-names" }
// { dg-do preprocess }

#include <ciso646>

// { dg-error "not a standard header" "" { target c++20 } 0 }

#ifdef and
# error "The header <ciso646> defines a macro named and"
#endif

#ifdef and_eq
# error "The header <ciso646> defines a macro named and_eq"
#endif

#ifdef bitand
# error "The header <ciso646> defines a macro named bitand"
#endif

#ifdef bitor
# error "The header <ciso646> defines a macro named bitor"
#endif

#ifdef compl
# error "The header <ciso646> defines a macro named compl"
#endif

#ifdef not
# error "The header <ciso646> defines a macro named not"
#endif

#ifdef not_eq
# error "The header <ciso646> defines a macro named not_eq"
#endif

#ifdef or
# error "The header <ciso646> defines a macro named or"
#endif

#ifdef or_eq
# error "The header <ciso646> defines a macro named or_eq"
#endif

#ifdef xor
# error "The header <ciso646> defines a macro named xor"
#endif

#ifdef xor_eq
# error "The header <ciso646> defines a macro named xor_eq"
#endif
