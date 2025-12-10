// { dg-do run { target c++20 } }

#include <debug/set>
#include <testsuite_hooks.h>

auto is_six = [](int p)
{ return p == 6; };

using __gnu_debug::set;

void test01()
{
  set<int> s;
  for (int i = 0; i != 10; ++i)
    s.insert(i);

  auto before = ++s.begin();
  auto match = std::next(s.begin(), 6);
  auto last = std::next(s.begin(), 9);

  VERIFY( std::erase_if(s, is_six) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(match._M_singular());
  VERIFY(last._M_dereferenceable());
}

int main()
{
  test01();
  return 0;
}
