// { dg-do compile { target c++14 } }
// { dg-require-effective-target net_ts_ip }

#include <experimental/internet>
#include <type_traits>

using namespace std;
using std::experimental::net::ip::udp;
using std::experimental::net::ip::basic_endpoint;
using std::experimental::net::ip::basic_resolver;
using std::experimental::net::basic_datagram_socket;
using std::experimental::net::basic_socket_acceptor;
using std::experimental::net::basic_socket_iostream;

void
test01()
{
  static_assert( ! is_default_constructible<udp>(), "" );
  static_assert( is_nothrow_copy_constructible<udp>(), "" );
  static_assert( is_nothrow_copy_assignable<udp>(), "" );

  static_assert( is_same<udp::endpoint, basic_endpoint<udp>>(), "");
  static_assert( is_same<udp::resolver, basic_resolver<udp>>(), "");
  static_assert( is_same<udp::socket,   basic_datagram_socket<udp>>(), "");

  static_assert( udp::v4() == udp::v4(), "" );
  static_assert( udp::v6() == udp::v6(), "" );
  static_assert( udp::v4() != udp::v6(), "" );
  static_assert( udp::v6() != udp::v4(), "" );

  static_assert( noexcept(udp::v6() == udp::v4()), "" );
  static_assert( noexcept(udp::v6() != udp::v4()), "" );

  static_assert( udp::v4().family() == AF_INET, "" );
  static_assert( udp::v6().family() == AF_INET6, "" );

  static_assert( udp::v4().type() == SOCK_DGRAM, "" );
  static_assert( udp::v6().type() == SOCK_DGRAM, "" );

  static_assert( udp::v4().protocol() == IPPROTO_UDP, "" );
  static_assert( udp::v6().protocol() == IPPROTO_UDP, "" );
}
