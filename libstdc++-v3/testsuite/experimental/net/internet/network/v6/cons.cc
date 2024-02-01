// { dg-do run { target c++14 } }
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <stdexcept>
#include <testsuite_hooks.h>

using std::experimental::net::ip::network_v6;
using std::experimental::net::ip::address_v6;

constexpr void
test01()
{
  network_v6 n0;
  VERIFY( n0.address().is_unspecified() );
  VERIFY( n0.prefix_length() == 0 );
}

constexpr void
test02()
{
  address_v6 a0;
  network_v6 n0{ a0, 0 };
  VERIFY( n0.address() == a0 );
  VERIFY( n0.prefix_length() == 0 );

  address_v6 a1{ address_v6::bytes_type{ 1, 2, 4, 8, 16, 32, 64, 128,
					 3, 7, 47, 71, 83, 165, 199, 255 } };
  network_v6 n1{ a1, 99};
  VERIFY( n1.address() == a1 );
  VERIFY( n1.prefix_length() == 99 );
}

void
test02_errors()
{
  address_v6 a0;
  try
  {
    network_v6{a0, -1};
    VERIFY(false);
  }
  catch(const std::out_of_range&)
  {
  }

  try
  {
    network_v6{a0, 129};
    VERIFY(false);
  }
  catch(const std::out_of_range&)
  {
  }
}

constexpr bool
test_constexpr()
{
  test01();
  test02();
  return true;
}

int
main()
{
  test01();
  test02();
  test02_errors();

  static_assert( test_constexpr(), "valid in constant expressions" );
}
