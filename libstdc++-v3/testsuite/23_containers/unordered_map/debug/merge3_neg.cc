// { dg-do run { target c++17 xfail *-*-* } }
// { dg-require-debug-mode "" }

#include <unordered_map>
#include <algorithm>
#include <testsuite_hooks.h>

using test_type = std::unordered_map<int, int>;

void
test01()
{
  test_type c0
    {
     { 1, 1 }, { 2, 2 }, { 3, 3 },
     { 5, 5 }, { 6, 6 }, { 7, 7 }
    };
  std::unordered_multimap<int, int> c1
    {
     { 1, 1 }, { 1, 1 }, { 2, 2 }, { 2, 2 },
     { 3, 3 }, { 3, 3 }, { 4, 4 }, { 4, 4 },
     { 5, 5 }
    };

  auto it1 = c1.find(1);
  auto it41 = c1.find(4);
  auto it42 = it41;
  ++it42;
  VERIFY( it42->second == 4 );

  c0.merge(c1);

  VERIFY( it1->second == 1 );
  VERIFY( c1.count(4) == 1 );
  VERIFY( it41 != it42 ); // Invalid iterator.
}

int
main()
{
  test01();
}
