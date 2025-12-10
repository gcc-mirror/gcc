// { dg-do run { target c++20 } }

#include <debug/map>
#include <testsuite_hooks.h>

auto is_six_pair = [](const std::pair<const int, int>& p)
{
  return p.first == 6;
};

using __gnu_debug::multimap;

void test01()
{
  multimap<int, int> m;
  for (int i = 0; i != 10; ++i)
    m.insert({ i, i });

  auto before = ++m.begin();
  auto match = std::next(m.begin(), 6);
  auto last = std::next(m.begin(), 9);

  VERIFY( std::erase_if(m, is_six_pair) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(match._M_singular());
  VERIFY(last._M_dereferenceable());
}

int main()
{
  test01();
  return 0;
}
