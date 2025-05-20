// { dg-do compile { target c++23 } }

// C++23 22.14.7.1 [format.range.fmtkind] p1: A program that instantiates
// the primary template of format_kind is ill-formed.

#include <format>

void test()
{
  (void) std::format_kind<void>; // { dg-error "here" }
  (void) std::format_kind<const void>; // { dg-error "here" }
  (void) std::format_kind<int>; // { dg-error "here" }
  (void) std::format_kind<int&>; // { dg-error "here" }
  (void) std::format_kind<const int(&)[10]>; // { dg-error "here" }
  (void) std::format_kind<void()>; // { dg-error "here" }
}

// { dg-error "cannot use primary template of 'std::format_kind'" "" { target *-*-* } 0 }
