// { dg-do run { target c++11 } }

// PR libstdc++/115399
// std::tr2::dynamic_bitset shift behaves differently from std::bitset

#include <tr2/dynamic_bitset>
#include <testsuite_hooks.h>

void
test_left_shift()
{
  std::tr2::dynamic_bitset<> b(65);
  b[0] = 1;
  auto b2 = b << 64;
  VERIFY(b2[64] == 1);
  VERIFY(b2[0] == 0);
  b <<= 64;
  VERIFY( b2 == b );
}

void
test_right_shift()
{
  std::tr2::dynamic_bitset<> b(65);
  b[64] = 1;
  auto b2 = b >> 64;
  VERIFY(b2[64] == 0);
  VERIFY(b2[0] == 1);
  b >>= 64;
  VERIFY( b2 == b );
}

int main()
{
  test_left_shift();
  test_right_shift();
}
