// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

// Resize
void test01()
{
  inplace_vector<int, 50> v(10, 17);
  v.reserve(20);

  auto before = v.begin() + 6;
  auto at = before + 1;
  auto after = at + 1;

  // Shrink.
  v.resize(7);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());
  VERIFY(after._M_singular());

  // Grow.
  before = v.begin() + 6;
  v.resize(17);
  VERIFY(before._M_dereferenceable());
}

int main()
{
  test01();
  return 0;
}
