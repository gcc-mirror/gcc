// { dg-options "-pedantic" }
// { dg-do compile { target c++11 } }
#include <array>
template class std::array<int, 0>;
// { dg-bogus "ambiguous overload" "" { target *-*-* } 0 }
