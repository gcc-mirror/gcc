// { dg-do run { target c++17 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_map>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_map<int, double>;

void
test01()
{
  test_type c0{ { 1, 3.5 }, { 2, 5.5 }, { 3, 7.5 }, { 5, 11.5 }, { 6, 13.5 } };
  test_type c1{ { 1, 3.5 }, { 2, 5.5 }, { 3, 7.5 }, { 4, 9.5 } };

  auto it2 = c1.find(2);
  auto it4 = c1.find(4);
  VERIFY( it2->second == 5.5 );
  VERIFY( it4->second == 9.5 );

  c0.merge(c1);

  VERIFY( it2->second == 5.5 );
  VERIFY( it4 != it2 ); // Invalid iterator.
}

int
main()
{
  test01();
}
