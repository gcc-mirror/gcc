// { dg-do run { target c++20 } }

#include <debug/vector>
#include <testsuite_hooks.h>

using __gnu_debug::vector;

void test01()
{
  vector<int> v;

  for (int i = 0; i != 10; ++i)
    v.push_back(i);

  auto before = v.begin() + 4;
  auto last = v.end() -1;

  VERIFY( std::erase(v, 6) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_singular());
}

int main()
{
  test01();
  return 0;
}
