// { dg-options "-std=gnu++2a" }
// { dg-do compile { xfail c++2a } }
#include <span>

// PR libstdc++/101411

void f(std::span<const int> s)
{
  std::as_writable_bytes(s); // { dg-error "unsatisfied constraints" }
}

void f1(std::span<const int, 1> s)
{
  std::as_writable_bytes(s); // { dg-error "unsatisfied constraints" }
}
