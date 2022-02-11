// { dg-do run { target c++17 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_map>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_map<int, int>;

void
test01()
{
  test_type c0{ { 1, 1 }, { 2, 2 }, { 3, 3 }, { 5, 5 }, { 6, 6 } };
  test_type c1{ { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };

  auto it2 = c1.find(2);
  auto it4 = c1.find(4);
  VERIFY( it2->second == 2 );
  VERIFY( it4->second == 4 );

  c0.merge(std::move(c1));

  VERIFY( it2->second == 2 );
  VERIFY( it2 != it4 ); // Invalid iterator.
}


int
main()
{
  test01();
}
