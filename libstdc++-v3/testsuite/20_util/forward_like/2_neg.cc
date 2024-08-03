// { dg-do compile { target c++23 } }

#include <utility>

auto x1 = std::forward_like<void>(1); // { dg-error "no match" }
// { dg-error "forming reference to void" "" { target *-*-* } 0 }
auto x2 = std::forward_like<void()const>(1); // { dg-error "no match" }
// { dg-error "forming reference to qualified function" "" { target *-*-* } 0 }
