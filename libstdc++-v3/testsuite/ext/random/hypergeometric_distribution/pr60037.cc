// { dg-options "-std=gnu++11 -O0" }
// { dg-require-cstdint "" }
// { dg-require-cmath "" }

#include <ext/random>
#include <functional>

void
hyperplot(unsigned int N, unsigned int K, unsigned int n)
{
  std::mt19937 re; // the default engine
  __gnu_cxx::hypergeometric_distribution<> hd(N, K, n);
  auto gen = std::bind(hd, re);
  gen();
}

int
main()
{
  hyperplot(15, 3, 2);
  hyperplot(500, 50, 30);
  hyperplot(100, 20, 5);
}
