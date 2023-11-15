// { dg-do compile { target c++23 } }

#include <memory>

std::shared_ptr<int> sp;
auto op = std::out_ptr(sp); // { dg-error "here" }
// { dg-error "deleter must be used" "" { target *-*-* } 0 }
