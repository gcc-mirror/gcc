// { dg-do run { target c++26 } }
// { dg-require-cstdint "" }

#include <random>

void
test01()
{
  unsigned long seed = 2;
  std::philox_engine<std::uint_fast32_t,
		     32, 4, 10, 0xCD9E8D57,
		     0x9E3779B9, 0xD2511F53,
		     0xBB67AE85> philox4x32seeded(seed);
}

int main()
{
  test01();
  return 0;
}
