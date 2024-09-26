// { dg-do compile { target c++11 } }
#include <functional>

int f();
auto b = std::bind<const int&>(f);
int i = b(); // { dg-error "here" "" { target { c++14_down } } }
// { dg-error "dangling reference" "" { target { c++14_down } } 0 }
// { dg-error "reference to temporary" "" { target { c++14_down } } 0 }
// { dg-error "no matching function" "" { target c++17 } 0 }
// { dg-error "enable_if" "" { target c++17 } 0 }
