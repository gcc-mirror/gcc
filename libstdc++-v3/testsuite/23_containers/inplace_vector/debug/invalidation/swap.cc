// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

void test01()
{
  inplace_vector<int, 10> v1;
  inplace_vector<int, 10> v2;

  for (int i = 0; i != 10; ++i)
    {
      v1.push_back(i);
      v2.push_back(i);
    }

  auto it1 = v1.begin();
  auto it2 = v2.begin();

  std::swap(v1, v2);

  VERIFY(it1._M_singular());
  VERIFY(it2._M_singular());
}

void test02()
{
  inplace_vector<int, 10> v1;
  inplace_vector<int, 10> v2;

  for (int i = 0; i != 10; ++i)
    {
      v1.push_back(i);
      v2.push_back(i);
    }

  auto it1 = v1.begin();
  auto it2 = v2.begin();

  swap(v1, v2);

  VERIFY(it1._M_singular());
  VERIFY(it2._M_singular());
}

int main()
{
  test01();
  test02();
  return 0;
}
