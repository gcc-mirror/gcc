// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

// Insert
void test01()
{
  inplace_vector<int, 100> v(10, 17);
  v.reserve(30);

  // Insert a single element
  auto before = v.begin() + 6;
  auto at = before + 1;
  auto after = at;
  at = v.insert(at, 42);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_singular());

  // Insert multiple copies
  before = v.begin() + 6;
  at = before + 1;
  v.insert(at, 3, 42);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());

  // Insert iterator range
  static int data[] = { 2, 3, 5, 7 };
  before = v.begin() + 6;
  at = before + 1;
  v.insert(at, &data[0], &data[0] + 4);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());
}

int main()
{
  test01();
  return 0;
}
