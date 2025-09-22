// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

// Assignment
void test01()
{
  inplace_vector<int, 30> v1;
  inplace_vector<int, 30> v2;

  auto i = v1.end();
  VERIFY(!i._M_dereferenceable() && !i._M_singular());

  v1 = v2;
  VERIFY(i._M_singular());

  i = v1.end();
  v1.assign(v2.begin(), v2.end());
  VERIFY( !i._M_singular() );

  i = v1.end();
  v1.assign(17, 42);
  VERIFY(i._M_singular());
}

int main()
{
  test01();
  return 0;
}
