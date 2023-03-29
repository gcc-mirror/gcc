// { dg-do compile { target c++17 } }
#include <tuple>
std::tuple<short> f();
auto t = std::make_from_tuple<const int&>(f()); // { dg-error "here" }
// { dg-error "static assertion failed" "" { target *-*-* } 0 }
