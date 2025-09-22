// { dg-do run { target c++26 } }

#include <debug/inplace_vector>
#include <testsuite_hooks.h>

using __gnu_debug::inplace_vector;

// Erase
void test04()
{
  inplace_vector<int, 30> v(20, 42);

  // Single element erase
  auto before = v.begin();
  auto at = before + 3;
  auto after = at;
  at = v.erase(at);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_dereferenceable());
  VERIFY(after._M_singular());

  // Multiple element erase
  before = v.begin();
  at = before + 3;
  v.erase(at, at + 3);
  VERIFY(before._M_dereferenceable());
  VERIFY(at._M_singular());

  // clear()
  before = v.begin();
  VERIFY(before._M_dereferenceable());
  v.clear();
  VERIFY(before._M_singular());
}

int main()
{
  test04();
  return 0;
}
