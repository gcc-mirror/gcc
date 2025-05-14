// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }
// PR libstdc++/70692
// No warning when function<const int&(...)> binds a reference to a temporary
#include <functional>

int f();

int main()
{
  std::function<const int&()> ff(f);  // { dg-error "no matching function" }
  std::function<long&&()> f2(f);      // { dg-error "no matching function" }
}
// { dg-error "std::enable_if" "" { target *-*-* } 0 }
