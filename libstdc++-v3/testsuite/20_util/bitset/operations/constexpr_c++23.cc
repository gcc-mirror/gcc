// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <bitset>
#include <testsuite_hooks.h>

constexpr bool
test()
{
  std::bitset<16> b0;
  std::bitset<16> b1 = ~b0;
  VERIFY( b1.all() );
  b0 &= b1;
  VERIFY( b0.none() );
  b0 |= b1;
  VERIFY( b0.all() );
  b0 ^= b1;
  VERIFY( b0.none() );
  b0 = b1 << 8;
  VERIFY( !b0.all() && !b0.none() );
  VERIFY( ((b1 << 8) | (b1 >> 8)).all() );
  b1 <<= 8;
  b1 >>= 8;
  b1 >>= 8;
  VERIFY( b1.none() );
  VERIFY( (~b1).all() );
  VERIFY( b1.flip().all() );
  return true;
}

static_assert( test() );
