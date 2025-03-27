// { dg-do compile { target c++20 } }

#include <format>

auto s = std::format(" {9} "); // { dg-error "call to consteval function" }
// { dg-error "invalid.arg.id" "" { target *-*-* } 0 }

struct X { };
std::format_string<X> str(""); // { dg-error "here" }
// { dg-error "std::formatter must be specialized" "" { target *-*-* } 0 }
