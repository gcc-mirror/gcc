// { dg-do run { target c++26 } }

// LWG 4444. Fix default template arguments for [...] ranges::replace_if

#include <algorithm>
#include <testsuite_hooks.h>

struct S { int i; int j; };
auto twenty = [](int i) { return i == 20; };

void
test_replace_if_iters()
{
  S s[2] = { {10, 11}, {20, 22} };
  std::ranges::replace_if(s, s+2, twenty, {30, 33}, &S::i);
  VERIFY( s[1].j == 33 );
}

void
test_replace_if_range()
{
  S s[2] = { {10, 11}, {20, 22} };
  std::ranges::replace_if(s, twenty, {40, 44}, &S::i);
  VERIFY( s[1].j == 44 );
}

int main()
{
  test_replace_if_iters();
  test_replace_if_range();
}
