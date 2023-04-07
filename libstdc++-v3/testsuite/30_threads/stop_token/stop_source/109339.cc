// { dg-options "-Wmaybe-uninitialized -Og -std=gnu++20" }
// { dg-do compile { target c++20 } }

#include <stop_token>

int main()
{
  std::stop_source ss;
 // { dg-bogus "uninitialized" "PR 109339" { target *-*-* } 0 }
}
