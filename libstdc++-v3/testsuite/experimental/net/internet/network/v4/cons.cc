// { dg-do run { target c++14 } }
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <stdexcept>
#include <testsuite_hooks.h>

using std::experimental::net::ip::network_v4;
using std::experimental::net::ip::address_v4;

constexpr void
test01()
{
  network_v4 n0;
  VERIFY( n0.address().is_unspecified() );
  VERIFY( n0.prefix_length() == 0 );
}

constexpr void
test02()
{
  address_v4 a0;
  network_v4 n0{ a0, 0 };
  VERIFY( n0.address() == a0 );
  VERIFY( n0.prefix_length() == 0 );

  address_v4 a1{ address_v4::bytes_type{ 1, 2, 3, 4 } };
  network_v4 n1{ a1, 12};
  VERIFY( n1.address() == a1 );
  VERIFY( n1.prefix_length() == 12 );
}

void
test02_errors()
{
  address_v4 a0;
  try
  {
    network_v4{a0, -1};
    VERIFY(false);
  }
  catch(const std::out_of_range&)
  {
  }

  try
  {
    network_v4{a0, 33};
    VERIFY(false);
  }
  catch(const std::out_of_range&)
  {
  }
}

constexpr void
test03()
{
  address_v4 a0;
  network_v4 n0{ a0, a0 };
  VERIFY( n0.address() == a0 );
  VERIFY( n0.prefix_length() == 0 );

  address_v4 a1{ address_v4::bytes_type{ 1, 2, 3, 4 } };
  network_v4 n1{ a1, address_v4::broadcast() };
  VERIFY( n1.address() == a1 );
  VERIFY( n1.prefix_length() == 32 );

  network_v4 n2{ a1, address_v4::bytes_type(128, 0, 0, 0) };
  VERIFY( n2.address() == a1 );
  VERIFY( n2.prefix_length() == 1 );

  network_v4 n3{ a1, address_v4::bytes_type(255, 255, 255, 192) };
  VERIFY( n3.address() == a1 );
  VERIFY( n3.prefix_length() == 26 );
}

void
test03_errors()
{
  address_v4 a0;
  try
  {
    // Contains non-contiguous non-zero bits.
    network_v4{a0, address_v4::bytes_type(255, 1, 0, 0)};
    VERIFY(false);
  }
  catch(const std::invalid_argument&)
  {
  }

  try
  {
    // Most significant bit is zero and any other bits are non-zero.
    network_v4{a0, address_v4::bytes_type(1, 0, 0, 0)};
    VERIFY(false);
  }
  catch(const std::invalid_argument&)
  {
  }

  try
  {
    // Most significant bit is zero and any other bits are non-zero.
    network_v4{a0, address_v4::bytes_type(0, 1, 0, 0)};
    VERIFY(false);
  }
  catch(const std::invalid_argument&)
  {
  }
}

constexpr bool
test_constexpr()
{
  test01();
  test02();
  test03();
  return true;
}


int
main()
{
  test01();
  test02();
  test02_errors();
  test03();
  test03_errors();

  static_assert( test_constexpr(), "valid in constant expressions" );
}
