// { dg-do run { target c++14 } }
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

using std::experimental::net::ip::network_v4;
using std::experimental::net::ip::address_v4;

constexpr void
test_netmask()
{
  network_v4 n0;
  VERIFY( n0.netmask() == address_v4() );

  network_v4 n1({}, 1);
  VERIFY( n1.netmask() == address_v4(address_v4::bytes_type(128)) );

  network_v4 n2({}, 2);
  VERIFY( n2.netmask() == address_v4(address_v4::bytes_type(192)) );

  network_v4 n3({}, 3);
  VERIFY( n3.netmask() == address_v4(address_v4::bytes_type(224)) );

  network_v4 n4({}, 17);
  VERIFY( n4.netmask() == address_v4(address_v4::bytes_type(255, 255, 128)) );
}

constexpr void
test_network()
{
  network_v4 n0;
  VERIFY( n0.network() == address_v4() );

  network_v4 n1(address_v4::bytes_type{1, 2, 3, 4}, 1);
  VERIFY( n1.network() == address_v4(address_v4::bytes_type(0, 0, 0, 0)) );

  network_v4 n2(address_v4::bytes_type{1, 2, 3, 4}, 8);
  VERIFY( n2.network() == address_v4(address_v4::bytes_type(1, 0, 0, 0)) );

  network_v4 n3(address_v4::bytes_type{1, 2, 3, 4}, 15);
  VERIFY( n3.network() == address_v4(address_v4::bytes_type(1, 2, 0, 0)) );

  network_v4 n4(address_v4::bytes_type{1, 2, 3, 4}, 16);
  VERIFY( n4.network() == address_v4(address_v4::bytes_type(1, 2, 0, 0)) );

  network_v4 n5(address_v4::bytes_type{1, 2, 3, 4}, 23);
  VERIFY( n5.network() == address_v4(address_v4::bytes_type(1, 2, 2, 0)) );

  network_v4 n6(address_v4::bytes_type{1, 2, 3, 4}, 24);
  VERIFY( n6.network() == address_v4(address_v4::bytes_type(1, 2, 3, 0)) );

  network_v4 n7(address_v4::bytes_type{1, 2, 3, 4}, 29);
  VERIFY( n7.network() == address_v4(address_v4::bytes_type(1, 2, 3, 0)) );

  network_v4 n8(address_v4::bytes_type{1, 2, 3, 4}, 30);
  VERIFY( n8.network() == address_v4(address_v4::bytes_type(1, 2, 3, 4)) );

  network_v4 n9(address_v4::bytes_type{1, 2, 3, 4}, 32);
  VERIFY( n9.network() == address_v4(address_v4::bytes_type(1, 2, 3, 4)) );
}

constexpr void
test_broadcast()
{
  using b = address_v4::bytes_type;

  network_v4 n0;
  VERIFY( n0.broadcast() == address_v4::broadcast() );

  network_v4 n1(b{1, 2, 3, 4}, 1);
  VERIFY( n1.broadcast() == address_v4(b(127, 255, 255, 255)) );

  network_v4 n2(b{1, 2, 3, 4}, 8);
  VERIFY( n2.broadcast() == address_v4(b(1, 255, 255, 255)) );

  network_v4 n3(b{1, 2, 3, 4}, 15);
  VERIFY( n3.broadcast() == address_v4(b(1, 3, 255, 255)) );

  network_v4 n4(b{1, 2, 3, 4}, 16);
  VERIFY( n4.broadcast() == address_v4(b(1, 2, 255, 255)) );

  network_v4 n5(b{1, 2, 3, 4}, 23);
  VERIFY( n5.broadcast() == address_v4(b(1, 2, 3, 255)) );

  network_v4 n6(b{1, 2, 3, 4}, 24);
  VERIFY( n6.broadcast() == address_v4(b(1, 2, 3, 255)) );

  network_v4 n7(b{1, 2, 3, 4}, 29);
  VERIFY( n7.broadcast() == address_v4(b(1, 2, 3, 7)) );

  network_v4 n8(b{1, 2, 3, 4}, 30);
  VERIFY( n8.broadcast() == address_v4(b(1, 2, 3, 7)) );

  network_v4 n9(b{1, 2, 3, 4}, 31);
  VERIFY( n9.broadcast() == address_v4(b(1, 2, 3, 5)) );

  network_v4 n10(b{1, 2, 3, 4}, 32);
  VERIFY( n10.broadcast() == address_v4(b(1, 2, 3, 4)) );
}

constexpr void
test_canonical()
{
  network_v4 n0;
  VERIFY( n0.canonical() == network_v4(n0.network(), n0.prefix_length()) );

  network_v4 n1(address_v4::bytes_type{1, 2, 3, 4}, 1);
  VERIFY( n1.canonical() == network_v4(n1.network(), n1.prefix_length()) );

  network_v4 n2(address_v4::bytes_type{1, 2, 3, 4}, 8);
  VERIFY( n2.canonical() == network_v4(n2.network(), n2.prefix_length()) );

  network_v4 n3(address_v4::bytes_type{1, 2, 3, 4}, 15);
  VERIFY( n3.canonical() == network_v4(n3.network(), n3.prefix_length()) );

  network_v4 n4(address_v4::bytes_type{1, 2, 3, 4}, 16);
  VERIFY( n4.canonical() == network_v4(n4.network(), n4.prefix_length()) );

  network_v4 n5(address_v4::bytes_type{1, 2, 3, 4}, 23);
  VERIFY( n5.canonical() == network_v4(n5.network(), n5.prefix_length()) );

  network_v4 n6(address_v4::bytes_type{1, 2, 3, 4}, 24);
  VERIFY( n6.canonical() == network_v4(n6.network(), n6.prefix_length()) );

  network_v4 n7(address_v4::bytes_type{1, 2, 3, 4}, 29);
  VERIFY( n7.canonical() == network_v4(n7.network(), n7.prefix_length()) );

  network_v4 n8(address_v4::bytes_type{1, 2, 3, 4}, 30);
  VERIFY( n8.canonical() == network_v4(n8.network(), n8.prefix_length()) );

  network_v4 n9(address_v4::bytes_type{1, 2, 3, 4}, 32);
  VERIFY( n9.canonical() == network_v4(n9.network(), n9.prefix_length()) );
}

constexpr void
test_is_host()
{
  network_v4 n0;
  VERIFY( ! n0.is_host() );

  network_v4 n1(address_v4::bytes_type{1, 2, 3, 4}, 1);
  VERIFY( ! n1.is_host() );

  network_v4 n2(address_v4::bytes_type{1, 2, 3, 4}, 8);
  VERIFY( ! n2.is_host() );

  network_v4 n3(address_v4::bytes_type{1, 2, 3, 4}, 32);
  VERIFY( n3.is_host() );
}

void
test_to_string()
{
  using b = address_v4::bytes_type;
  __gnu_test::uneq_allocator<char> alloc(123);
  auto str = network_v4(address_v4(b(12, 34, 56, 78)), 24).to_string(alloc);
  VERIFY(str.get_allocator().get_personality() == alloc.get_personality());
  VERIFY( str == "12.34.56.78/24" );

  __gnu_test::uneq_allocator<char> alloc2(99);
  auto str2 = network_v4(address_v4(b(87, 65, 43, 21)), 4).to_string(alloc2);
  VERIFY(str2.get_allocator().get_personality() == alloc2.get_personality());
  VERIFY( str2 == "87.65.43.21/4" );
}

constexpr bool
test_constexpr()
{
  test_netmask();
  test_network();
  test_broadcast();
  test_canonical();
  test_is_host();
  return true;
}

int main()
{
  test_netmask();
  test_network();
  test_broadcast();
  test_canonical();
  test_is_host();
  test_to_string();

  static_assert( test_constexpr(), "valid in constant expressions" );
}
