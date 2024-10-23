// { dg-options "-D_GLIBCXX_USE_DEPRECATED=0 -Wdeprecated" }
// { dg-do compile }

#include <ccomplex>

std::complex<double> d;

// { dg-error "ISO C.. 2011" "" { target c++98_only } 0 }
// { dg-warning "deprecated" "" { target c++17_only } 0 }
// { dg-error "not a standard header" "" { target c++20 } 0 }
