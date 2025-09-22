// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

void test01()
{
  inplace_vector<int, 100> v(10, 17);

  auto before = v.begin() + 6;
  auto last = v.end();
  auto end = last--;

  VERIFY( v.try_push_back(42) != nullptr );

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_dereferenceable());
  VERIFY(end._M_singular());
}

void test02()
{
  std::vector<int> vv { 0, 1, 2, 3, 4, 5 };
  inplace_vector<std::vector<int>, 100> v(10, vv);

  auto before = v.begin() + 6;
  auto last = v.end();
  auto end = last--;

  VERIFY( v.try_push_back(std::move(vv)) != nullptr );

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_dereferenceable());
  VERIFY(end._M_singular());
}

int main()
{
  test01();
  test02();
  return 0;
}
