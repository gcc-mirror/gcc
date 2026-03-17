// { dg-do run { target c++26 } }

// LWG 4444. Fix default template arguments for ranges::replace [...]

#include <algorithm>
#include <testsuite_hooks.h>

struct S { int i; int j; };

void
test_replace_iters()
{
  S s[2] = { {10, 11}, {20, 22} };
  std::ranges::replace(s, s+2, 20, {30, 33}, &S::i);
  VERIFY( s[1].j == 33 );
}

void
test_replace_range()
{
  S s[2] = { {10, 11}, {20, 22} };
  std::ranges::replace(s, 20, {40, 44}, &S::i);
  VERIFY( s[1].j == 44 );
}

int main()
{
  test_replace_iters();
  test_replace_range();
}
