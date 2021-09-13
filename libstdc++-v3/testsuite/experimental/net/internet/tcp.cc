// { dg-do compile { target c++14 } }
// { dg-require-effective-target net_ts_ip }

#include <experimental/internet>
#include <type_traits>

using namespace std;
using std::experimental::net::ip::tcp;
using std::experimental::net::ip::basic_endpoint;
using std::experimental::net::ip::basic_resolver;
using std::experimental::net::basic_stream_socket;
using std::experimental::net::basic_socket_acceptor;
using std::experimental::net::basic_socket_iostream;

void
test01()
{
  static_assert( ! is_default_constructible<tcp>(), "" );
  static_assert( is_nothrow_copy_constructible<tcp>(), "" );
  static_assert( is_nothrow_copy_assignable<tcp>(), "" );

  static_assert( is_same<tcp::endpoint, basic_endpoint<tcp>>(), "");
  static_assert( is_same<tcp::resolver, basic_resolver<tcp>>(), "");
  static_assert( is_same<tcp::socket,   basic_stream_socket<tcp>>(), "");
  static_assert( is_same<tcp::acceptor, basic_socket_acceptor<tcp>>(), "");
  static_assert( is_same<tcp::iostream, basic_socket_iostream<tcp>>(), "");

  static_assert( tcp::v4() == tcp::v4(), "" );
  static_assert( tcp::v6() == tcp::v6(), "" );
  static_assert( tcp::v4() != tcp::v6(), "" );
  static_assert( tcp::v6() != tcp::v4(), "" );

  static_assert( noexcept(tcp::v6() == tcp::v4()), "" );
  static_assert( noexcept(tcp::v6() != tcp::v4()), "" );

  static_assert( tcp::v4().family() == AF_INET, "" );
  static_assert( tcp::v6().family() == AF_INET6, "" );

  static_assert( tcp::v4().type() == SOCK_STREAM, "" );
  static_assert( tcp::v6().type() == SOCK_STREAM, "" );

  static_assert( tcp::v4().protocol() == IPPROTO_TCP, "" );
  static_assert( tcp::v6().protocol() == IPPROTO_TCP, "" );
}
