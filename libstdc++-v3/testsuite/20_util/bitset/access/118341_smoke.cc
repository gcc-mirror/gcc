// { dg-do run }
// { dg-options "-D_GLIBCXX_ASSERTIONS" }

// Smoke test, op[] hardening.

#include <bitset>
#include <testsuite_hooks.h>

void test_non_const_subscript()
{
  std::bitset<13> bs(0x1555ull);
  for (int i = 0; i < 13; ++i)
    {
      VERIFY(bs[i] != (i & 1)); // Check op[] proxy result rvalue.
      bs[i] = not bs[i];        // Assign via op[] proxy result lvalue.
      VERIFY(bs[i] == (i & 1)); // Check modified.
    }
}

void test_const_subscript()
{
  const std::bitset<13> cbs(0x1555ull);
  for (int i = 0; i < 13; ++i)
    VERIFY(cbs[i] != (i & 1));  // Check op[] proxy result const rvalue.
}

int main()
{
  test_non_const_subscript();
  test_const_subscript();
}
