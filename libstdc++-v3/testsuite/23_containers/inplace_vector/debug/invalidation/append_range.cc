// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

void test01()
{
  inplace_vector<int, 100> v(10, 17);
  inplace_vector<int, 10> v1(10, 19);

  auto before = v.begin() + 6;
  auto last = v.end();
  auto end = last--;

  v.append_range(v1);

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_dereferenceable());
  VERIFY(end._M_singular());
}

void test02()
{
  inplace_vector<int, 100> v(10, 17);
  inplace_vector<int, 0> v1;

  auto before = v.begin() + 6;
  auto last = v.end();
  auto end = last--;

  v.append_range(v1);

  VERIFY(before._M_dereferenceable());
  VERIFY(last._M_dereferenceable());
  VERIFY(!end._M_singular());
}

int main()
{
  test01();
  test02();
  return 0;
}
