// { dg-do run { xfail *-*-* } }
// { dg-options "-D_GLIBCXX_ASSERTIONS" }

#include <bitset>
#include <testsuite_hooks.h>

// Check bitset<>::op[] hardening, const.

int main()
{
  const std::bitset<13> bs(0x1555ull);
  bs[12];  // OK
  bs[13];  // aborts, 13 > 12, const
}
