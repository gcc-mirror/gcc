// { dg-do run { target c++26 } }

// N5014 29.5.6 Engines and engine adaptors with predefined parameters

#include <random>
#include <testsuite_hooks.h>

using test_type = std::philox_engine<std::uint_fast64_t, 64, 4, 10,
				     0xCA5A826395121157, 0x9E3779B97F4A7C15,
				     0xD2E7470EE14C6C93, 0xBB67AE8584CAA73B>;
static_assert( std::is_same_v<test_type, std::philox4x64> );

void
test01()
{
  std::philox4x64 a;
  a.discard(9999);

  VERIFY( a() == 3409172418970261260 );
}

int main()
{
  test01();
}
