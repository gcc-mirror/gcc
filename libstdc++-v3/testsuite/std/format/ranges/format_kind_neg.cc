// { dg-do compile { target c++23 } }

// C++23 22.14.7.1 [format.range.fmtkind] p1: A program that instantiates
// the primary template of format_kind is ill-formed.

#include <format>

template<auto> struct Tester { };

Tester<std::format_kind<const int(&)[1]>> t; // { dg-error "here" }

// { dg-error "use of 'std::format_kind" "" { target *-*-* } 0 }
// { dg-error "primary_template_not_defined" "" { target *-*-* } 0 }
