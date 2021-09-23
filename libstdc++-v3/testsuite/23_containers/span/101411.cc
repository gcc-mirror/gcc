// { dg-options "-std=gnu++20" }
// { dg-do compile { xfail c++20 } }
#include <span>

// PR libstdc++/101411

void f(std::span<const int> s)
{
  std::as_writable_bytes(s); // { dg-error "no matching function" }
}

void f1(std::span<const int, 1> s)
{
  std::as_writable_bytes(s); // { dg-error "no matching function" }
}
