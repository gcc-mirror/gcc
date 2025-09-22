// { dg-do run { target c++26 } }
// { dg-require-debug-mode "" }

#include <inplace_vector>
#include <testsuite_hooks.h>

void test01()
{
  std::inplace_vector<int, 10> v;

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
  std::inplace_vector<int, 0> v;

  VERIFY( std::erase(v, 6) == 0 );
}

int main()
{
  test01();
  test02();
  return 0;
}
