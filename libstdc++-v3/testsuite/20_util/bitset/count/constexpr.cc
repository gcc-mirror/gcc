// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <bitset>
#include <testsuite_hooks.h>

constexpr bool
test_counting()
{
  auto check = []<std::size_t N>(const std::bitset<N>& bs) {
    VERIFY( bs.size() == N );
    unsigned count = 0;
    for (unsigned n = 0; n < N; ++n)
      if (bs.test(n))
	++count;
    VERIFY( count == bs.count() );
    VERIFY( bs.all() == (bs.count() == bs.size()) );
    VERIFY( bs.any() == (bs.count() != 0) );
    VERIFY( bs.none() == (bs.count() == 0) );
    return true;
  };

  std::bitset<0> z0;
  VERIFY( z0.count() == 0 );
  VERIFY( check(z0) );
  z0.set();
  VERIFY( z0.count() == 0 );
  VERIFY( check(z0) );

  std::bitset<7> z7;
  VERIFY( z7.count() == 0 );
  VERIFY( check(z7) );
  z7.set();
  VERIFY( z7.count() == 7 );
  VERIFY( check(z7) );
  z7.flip(1);
  VERIFY( z7.count() == 6 );
  VERIFY( check(z7) );

  std::bitset<31> z31;
  VERIFY( z31.count() == 0 );
  VERIFY( check(z31) );
  z31.set();
  VERIFY( z31.count() == 31 );
  VERIFY( check(z31) );
  z31.flip(1);
  VERIFY( z31.count() == 30 );
  VERIFY( check(z31) );

  std::bitset<32> z32;
  VERIFY( z32.count() == 0 );
  VERIFY( check(z32) );
  z32.set();
  VERIFY( z32.count() == 32 );
  VERIFY( check(z32) );
  z32.flip(1);
  VERIFY( z32.count() == 31 );
  VERIFY( check(z32) );

  std::bitset<63> z63;
  VERIFY( z63.count() == 0 );
  VERIFY( check(z63) );
  z63.set();
  VERIFY( z63.count() == 63 );
  VERIFY( check(z63) );
  z63.flip(1);
  VERIFY( z63.count() == 62 );
  VERIFY( check(z63) );

  std::bitset<64> z64;
  VERIFY( z64.count() == 0 );
  VERIFY( check(z64) );
  z64.set();
  VERIFY( z64.count() == 64 );
  VERIFY( check(z64) );
  z64.flip(1);
  VERIFY( z64.count() == 63 );
  VERIFY( check(z64) );

  std::bitset<1000> z1k;
  VERIFY( z1k.count() == 0 );
  VERIFY( check(z1k) );
  z1k.set();
  VERIFY( z1k.count() == 1000 );
  VERIFY( check(z1k) );
  z1k.flip(1);
  VERIFY( z1k.count() == 999 );
  VERIFY( check(z1k) );

  return true;
}

static_assert( test_counting() );
