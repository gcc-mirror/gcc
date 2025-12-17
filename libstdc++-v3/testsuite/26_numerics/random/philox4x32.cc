// { dg-do run { target c++26 } }

// N5014 29.5.6 Engines and engine adaptors with predefined parameters

#include <random>
#include <testsuite_hooks.h>

using test_type = std::philox_engine<std::uint_fast32_t, 32, 4, 10,
				     0xCD9E8D57, 0x9E3779B9,
				     0xD2511F53, 0xBB67AE85>;
static_assert( std::is_same_v<test_type, std::philox4x32> );

void
test01()
{
  std::philox4x32 a;
  a.discard(9999);

  VERIFY( a() == 1955073260 );
}

int main()
{
  test01();
}
