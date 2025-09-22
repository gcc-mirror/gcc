// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

void test01()
{
  inplace_vector<int, 10> v;

  for (int i = 0; i != 10; ++i)
    v.push_back(i);

  auto before = v.begin() + 4;
  auto last = v.end() - 1;

  VERIFY( std::erase(v, 6) == 1 );

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_singular());
}

void test02()
{
  inplace_vector<int, 0> v;

  VERIFY( std::erase(v, 6) == 0 );
}

int main()
{
  test01();
  test02();
  return 0;
}
