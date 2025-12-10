// { dg-do run { target c++20 } }
// { dg-require-debug-mode "" }

#include <unordered_set>
#include <testsuite_hooks.h>

auto is_six = [](int p)
{ return p == 6; };

void test01()
{
  std::unordered_set<int> s;
  for (int i = 0; i != 10; ++i)
    s.insert(i);

  auto before = s.find(1);
  auto match = s.find(6);
  auto last = s.find(9);

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
