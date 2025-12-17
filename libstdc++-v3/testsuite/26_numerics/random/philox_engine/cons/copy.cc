// { dg-do run { target c++26 } }

// N5014 29.5.4 Random Number Engine Class Templates
// N5014 29.5.4.5 Class Template philox_engine

#include <random>
#include <testsuite_hooks.h>

void
test01(unsigned long seed)
{

  std::philox_engine<std::uint_fast32_t, 32, 4, 10, 0xCD9E8D57,
	0x9E3779B9, 0xD2511F53, 0xBB67AE85> e(seed);

  const auto f(e);
  VERIFY( f == e );
  auto g(f);
  VERIFY( g == f );
}

int main()
{
  test01(1ul);
  test01(111ul);
}
