// { dg-do run { target c++26 } }
// { dg-require-cstdint "" }

// 29.5.4 Random Number Engine Class Templates
// 29.5.4.5 Class Template philox_engine

#include <random>

void test01()
{
  std::philox4x32 philox;
  const void* p = &philox.word_size;
  p = &philox.word_count;
  p = &philox.round_count;
  p = &philox.multipliers;
  p = &philox.round_consts;
  p = &philox.default_seed;
  p = p; // Suppress unused warning.
}

int
main()
{
  test01();
  return 0;
}
