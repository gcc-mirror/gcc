// { dg-do run { target c++17 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_set>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_set<int>;

void
test01()
{
  test_type c0{ 1, 2, 3, 5, 6 };
  test_type c1{ 1, 2, 3, 4 };

  auto it2 = c1.find(2);
  auto it4 = c1.find(4);
  VERIFY( *it2 == 2 );
  VERIFY( *it4 == 4 );

  c0.merge(c1);

  VERIFY( *it2 == 2 );
  VERIFY( it2 != it4 ); // Invalid iterator.
}

int
main()
{
  test01();
}
