// { dg-do run { target c++26 } }

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

void
test01()
{
  const int f1 = f(0);
  const int f2 = f(0);

  const int g1 = g(0);
  const int g2 = g(0);

  VERIFY( f1 == f2 );
  VERIFY( g1 == g2 );
  VERIFY( f1 == g1 );
}

void
test02()
{
  std::philox4x64 e1(25);
  std::philox4x64 e2;
  VERIFY( e2 != e1 );
  e2.seed(25);
  VERIFY( e2 == e1 );

}

int main()
{
  test01();
  test02();
}
