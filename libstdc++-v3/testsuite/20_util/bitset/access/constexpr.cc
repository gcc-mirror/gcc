// { dg-options "-std=gnu++23" }
// { dg-do compile { target c++23 } }

#include <bitset>
#include <string>
#include <testsuite_hooks.h>

constexpr bool
test_indexing()
{
  std::bitset<100> b("10010110");
  VERIFY( b[0] == 0 );
  VERIFY( b[1] == 1 );
  const auto& cb = b;
  VERIFY( cb[0] == 0 );
  VERIFY( cb[1] == 1 );
  b[1].flip();
  VERIFY( cb[1] == 0 );
  VERIFY( b[1] == 0 );
  VERIFY( ~b[1] == 1 );
  b[3] = true;
  bool b3 = b[3];
  VERIFY( b3 );
  b[4] = b[3];
  return true;
}

static_assert( test_indexing() );

#if _GLIBCXX_USE_CXX11_ABI
constexpr bool
test_to_string()
{
  std::string str = "01101001";
  return std::bitset<8>(str).to_string() == str;
}

static_assert( test_to_string() );
#endif

constexpr bool
test_to_ulong()
{
  unsigned long val = 0xcabba123;
  return std::bitset<100>(val).to_ulong() == val;
}

static_assert( test_to_ulong() );

constexpr bool
test_to_ullong()
{
  unsigned long long val = 0x0123abcd0123abcd;
  return std::bitset<100>(val).to_ullong() == val;
}

static_assert( test_to_ullong() );
