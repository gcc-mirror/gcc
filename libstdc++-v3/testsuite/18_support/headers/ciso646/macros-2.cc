// { dg-options " -Wdeprecated -fno-operator-names" }
// { dg-do preprocess }

// Should get a warning for C++20 and up without -D_GLIBCXX_USE_DEPRECATED=0
// { dg-warning "not a standard header" "" { target c++20 } 0 }

#include "macros.cc"
