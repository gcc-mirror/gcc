// { dg-do compile { target c++23 } }

#include <memory>

std::shared_ptr<int> sp;
auto iop = std::inout_ptr(sp); // { dg-error "here" }
std::shared_ptr<long> sp2;
std::default_delete<int> d;
auto iop2 = std::inout_ptr(sp2, d); // { dg-error "here" }

// { dg-error "can not be used" "" { target *-*-* } 0 }
