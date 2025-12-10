// { dg-do run { target c++20 } }

#include <debug/forward_list>
#include <testsuite_hooks.h>

using __gnu_debug::forward_list;

void test01()
{
  forward_list<int> fl({ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 });

  auto before = ++fl.begin();
  auto match = std::next(fl.begin(), 6);
  auto last = std::next(fl.begin(), 9);

  VERIFY( std::erase(fl, 6) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(match._M_singular());
  VERIFY(last._M_dereferenceable());
}

int main()
{
  test01();
  return 0;
}
