// { dg-do run { target c++20 } }

// LWG 3865. Sorting a range of pairs

#include <utility>
#include <testsuite_hooks.h>

int main()
{
  std::pair<int, int> p(1, 2);
  std::pair<int&, int&> p2(p.first, p.second);
  VERIFY( p == p2 );
  VERIFY( p <= p2 );
  VERIFY( p >= p2 );
}
