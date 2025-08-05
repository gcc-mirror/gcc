// { dg-do run { target c++26 } }
// { dg-require-cstdint "" }

// 29.5.4 Random Number Engine Class Templates
// 29.5.4.5 Class Template philox_engine

#include <random>
#include <testsuite_hooks.h>

void
test01()
{
  std::philox4x32 a;
  a.discard(9999);

  VERIFY( a() == 1955073260 );
}

int main()
{
  test01();
  return 0;
}
