// { dg-do run { target c++14 } }
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <testsuite_hooks.h>

using namespace std::experimental::net;

constexpr void
test_default()
{
  ip::tcp::endpoint t1;
  VERIFY( t1.protocol() == ip::tcp::v4() );
  VERIFY( t1.address() == ip::address() );
  VERIFY( t1.port() == 0 );

  ip::udp::endpoint t2;
  VERIFY( t2.protocol() == ip::udp::v4() );
  VERIFY( t2.address() == ip::address() );
  VERIFY( t2.port() == 0 );
}

constexpr void
test_proto()
{
  ip::tcp::endpoint t1(ip::tcp::v4(), 22);
  VERIFY( t1.protocol() == ip::tcp::v4() );
  VERIFY( t1.address() == ip::address_v4() );
  VERIFY( t1.port() == 22 );

  ip::tcp::endpoint t2(ip::tcp::v6(), 80);
  VERIFY( t2.protocol() == ip::tcp::v6() );
  VERIFY( t2.address() == ip::address_v6() );
  VERIFY( t2.port() == 80 );
}

constexpr void
test_addr()
{
  ip::address_v4 a1(ip::address_v4::bytes_type(1, 2, 3, 4));
  ip::tcp::endpoint t1(a1, 22);
  VERIFY( t1.protocol() == ip::tcp::v4() );
  VERIFY( t1.address() == a1 );
  VERIFY( t1.port() == 22 );

  ip::address_v6 a2(ip::address_v6::bytes_type(21,22,23,24,25,26,27,28,29));
  ip::tcp::endpoint t2(a2, 80);
  VERIFY( t2.protocol() == ip::tcp::v6() );
  VERIFY( t2.address() == a2 );
  VERIFY( t2.port() == 80 );
}

int main()
{
  test_default();
  test_proto();
  test_addr();

  constexpr bool c = [] {
    test_default();
    test_proto();
    test_addr();
    return true;
  };
}
