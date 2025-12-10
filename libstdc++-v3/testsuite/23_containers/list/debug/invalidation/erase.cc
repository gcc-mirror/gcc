// { dg-do run { target c++20 } }

#include <debug/list>
#include <testsuite_hooks.h>

using __gnu_debug::list;

void test01()
{
  list<int> l;

  for (int i = 0; i != 10; ++i)
    l.push_back(i);

  auto before = ++l.begin();
  auto match = std::next(l.begin(), 6);
  auto last = --l.end();

  VERIFY( std::erase(l, 6) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(match._M_singular());
  VERIFY(last._M_dereferenceable());
}

int main()
{
  test01();
  return 0;
}
