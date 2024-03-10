// { dg-do compile { target c++20 } }

#include <format>

auto s = std::format(" {9} ");
// { dg-error "invalid.arg.id" "" { target *-*-* } 0 }
