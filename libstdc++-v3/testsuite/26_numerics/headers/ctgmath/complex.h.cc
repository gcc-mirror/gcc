// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated" }
// { dg-do compile }
// { dg-bogus "deprecated" "C++17 deprecated <ctgmath> but not <tgmath.h>" }

#include <tgmath.h>

#if __cplusplus < 201103L
# undef complex
#endif
std::complex<double> d; // { dg-error "does not name a template" "" { target c++98_only } 0 }
