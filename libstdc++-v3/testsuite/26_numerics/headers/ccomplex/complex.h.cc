// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated" }
// { dg-do compile }
// { dg-bogus "deprecated" "C++17 deprecated <ccomplex> but not <complex.h>" }

#include <complex.h>

#if __cplusplus < 201103L
# undef complex
#endif
std::complex<double> d; // { dg-error "does not name a template" "" { target c++98_only } 0 }
