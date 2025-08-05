// { dg-do run { target c++26 } }
// { dg-require-cstdint "" }

#include <random>
#include <testsuite_hooks.h>

int f(int x)
{
  std::seed_seq sq(&x, &x + 1);
  auto rnd = std::philox4x32(sq);
  return std::uniform_int_distribution<int>()(rnd);
}

int g(int x)
{
  std::seed_seq sq(&x, &x + 1);
  auto rnd = std::philox4x32();
  rnd.seed(sq);
  return std::uniform_int_distribution<int>()(rnd);
}

void test01()
{
  const int f1 = f(0);
  const int f2 = f(0);

  const int g1 = g(0);
  const int g2 = g(0);

  VERIFY( f1 == f2 );
  VERIFY( g1 == g2 );
  VERIFY( f1 == g1 );
}

int main()
{
  test01();
  return 0;
}
