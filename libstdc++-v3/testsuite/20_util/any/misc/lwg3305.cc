// { dg-do compile { target c++17 } }

// LWG 3305. any_cast<void>

#include <any>

void
test_lwg3305()
{
  std::any a;
  (void) std::any_cast<const void>(&a); // { dg-error "here" }
  const std::any a2;
  (void) std::any_cast<volatile void>(&a2); // { dg-error "here" }
}
// { dg-error "static assertion failed" "" { target *-*-* } 0 }
