// { dg-do run { target c++17 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_set>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_multiset<int>;

void
test01()
{
  test_type c0{ 1, 1, 2, 2, 3, 3 };
  std::unordered_set<int> c1{ 1, 2, 3 };

  auto it = c1.find(2);
  VERIFY( *it == 2 );

  c0.merge(std::move(c1));

  VERIFY( it != c1.end() ); // Invalid iterator.
}

int
main()
{
  test01();
}
