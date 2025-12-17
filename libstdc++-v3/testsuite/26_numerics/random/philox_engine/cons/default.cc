// { dg-do run { target c++26 } }

// N5014 29.5.4.5 Class Template philox_engine

#include <random>
#include <testsuite_hooks.h>

void
test_default_ctor()
{
  std::philox_engine<std::uint_fast32_t,
		     32, 4, 10, 0xCD9E8D57,
		     0x9E3779B9, 0xD2511F53,
		     0xBB67AE85> philox4x32nullkey(0);

  VERIFY( philox4x32nullkey.min() == 0 );
  VERIFY( philox4x32nullkey.max() == (1ul << 31 | (1ul << 31) - 1) );
  VERIFY( philox4x32nullkey() == 0x6627e8d5ul );
}

void
test_seed()
{
  unsigned long seed = 2;
  std::philox4x32 seeded(seed);

  std::philox4x32 default_init;
  VERIFY( seeded != default_init );

  std::philox4x32 default_seeded(std::philox4x32::default_seed);
  VERIFY( default_seeded == default_init );
}

void
test_seed_seq()
{
  std::seed_seq sseq{ 1, 2, 3, 4 };
  std::philox4x32 seeded(sseq);
  std::philox4x32 default_init;
  VERIFY( seeded != default_init );
}

int main()
{
  test_default_ctor();
  test_seed();
  test_seed_seq();
}
