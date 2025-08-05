// { dg-do run { target c++26 } }
// { dg-require-cstdint "" }

// 29.5.4 Random Number Engine Class Templates
// 29.5.4.5 Class Template philox_engine

#include <random>

void
test01()
{

  std::philox_engine<std::uint_fast32_t, 32, 4, 10, 0xCD9E8D57,
	0x9E3779B9, 0xD2511F53, 0xBB67AE85> e(1ul);

  const auto f(e);
  auto g(f);
  g = g; // Suppress unused warning
}

int main()
{
  test01();
  return 0;
}
