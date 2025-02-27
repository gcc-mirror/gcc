// { dg-do compile { target c++23 } }

// Bug 112803 - <ranges>: to(Args&&... args) is missing Mandates

#include <ranges>

void
test()
{
  struct C { };

  (void) std::ranges::to<int>(); // { dg-error "here" }
  (void) std::ranges::to<C*>(); // { dg-error "here" }
  (void) std::ranges::to<C&>(); // { dg-error "here" }
  (void) std::ranges::to<const C>(); // { dg-error "here" }
  (void) std::ranges::to<volatile C>(); // { dg-error "here" }
  (void) std::ranges::to<const volatile C>(); // { dg-error "here" }
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
