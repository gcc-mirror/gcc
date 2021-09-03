// { dg-do run { target c++14 } }
// { dg-require-effective-target net_ts_ip }

#include <experimental/internet>
#include <testsuite_common_types.h>
#include <testsuite_hooks.h>

using namespace std;

template<typename C, typename T, typename P>
void check_gettable_sockopt(P p, C c = C())
{
  static_assert( is_same<decltype(declval<const C&>().level(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().level(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().name(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().name(p)), "" );

  static_assert( is_same<decltype(declval<C&>().data(p)), void*>(), "" );
  static_assert( noexcept(declval<C&>().data(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().size(p)), size_t>(), "" );
  static_assert( noexcept(declval<const C&>().size(p)), "" );

  static_assert( is_same<decltype(declval<C&>().resize(p, 0)), void>(), "" );
  static_assert( ! noexcept(declval<C&>().resize(p, 0)), "" );

  VERIFY(c.size(p) == sizeof(T));
}

template<typename C, typename T, typename P>
void check_settable_sockopt(P p, C c = C())
{
  static_assert( is_same<decltype(declval<const C&>().level(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().level(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().name(p)), int>(), "" );
  static_assert( noexcept(declval<const C&>().name(p)), "" );

  static_assert( is_same<decltype(declval<const C&>().data(p)), const void*>(), "" );
  static_assert( noexcept(declval<const C&>().data(p)), "" );

  static_assert( is_same<decltype(declval<C&>().size(p)), size_t>(), "" );
  static_assert( noexcept(declval<C&>().size(p)), "" );

  VERIFY(c.size(p) == sizeof(T));
}

template<typename C, typename T = int>
void check_boolean_sockopt()
{
  namespace ip = std::experimental::net::ip;
#if __has_include(<netinet/in.h>)
  check_gettable_sockopt<C, T>(ip::tcp::v4());
  check_settable_sockopt<C, T>(ip::tcp::v4());
#endif

  static_assert( is_destructible<C>(), "" );
  static_assert( is_nothrow_default_constructible<C>(), "" );
  static_assert( is_nothrow_copy_constructible<C>(), "" );
  static_assert( is_nothrow_copy_assignable<C>(), "" );

  static_assert( is_nothrow_constructible<C, bool>(), "" );
  static_assert( is_nothrow_assignable<C&, bool>(), "" );

  static_assert( is_same<decltype(declval<const C&>().value()), bool>(), "" );
  static_assert( noexcept(declval<const C&>().value()), "" );

  static_assert( is_same<decltype(static_cast<bool>(declval<const C&>())), bool>(), "" );
  static_assert( noexcept(static_cast<bool>(declval<const C&>())), "" );

  static_assert( is_same<decltype(!declval<const C&>()), bool>(), "" );
  static_assert( noexcept(!declval<const C&>()), "" );
}

template<typename C, typename T = int>
void check_integer_sockopt()
{
  namespace ip = std::experimental::net::ip;
#if __has_include(<netinet/in.h>)
  check_gettable_sockopt<C, T>(ip::tcp::v4());
  check_settable_sockopt<C, T>(ip::tcp::v4());
#endif

  static_assert( is_destructible<C>(), "" );
  static_assert( is_nothrow_default_constructible<C>(), "" );
  static_assert( is_nothrow_copy_constructible<C>(), "" );
  static_assert( is_nothrow_copy_assignable<C>(), "" );

  static_assert( is_nothrow_constructible<C, int>(), "" );
  static_assert( is_nothrow_assignable<C&, int>(), "" );

  static_assert( is_same<decltype(declval<const C&>().value()), int>(), "" );
  static_assert( noexcept(declval<const C&>().value()), "" );
}

template<typename C>
void check_mcast_sockopt(C& c)
{
  namespace ip = std::experimental::net::ip;
  static_assert( is_destructible<C>(), "" );
  static_assert( is_copy_constructible<C>(), "" );
  static_assert( is_copy_assignable<C>(), "" );

#if __has_include(<netinet/in.h>)
  check_settable_sockopt<C, ipv6_mreq>(ip::tcp::v6(), c);
  check_settable_sockopt<C, ip_mreq>(ip::tcp::v4(), c);
#endif

  static_assert( is_nothrow_constructible<C, const ip::address&>(), "" );
  static_assert( ! is_convertible<const ip::address&, C>(), "explicit" );
  static_assert( is_nothrow_constructible<C, const ip::address_v4&>(), "" );
  static_assert( ! is_convertible<const ip::address_v4&, C>(), "explicit" );
  static_assert( is_nothrow_constructible<C, const ip::address_v4&, const ip::address_v4&>(), "" );
  static_assert( is_nothrow_constructible<C, const ip::address_v6&>(), "" );
  static_assert( ! is_convertible<const ip::address_v6&, C>(), "explicit" );
  static_assert( is_nothrow_constructible<C, const ip::address_v6&, unsigned>(), "" );
}

void test_option_types()
{
  namespace ip = std::experimental::net::ip;

#if __has_include(<netinet/in.h>)
#if __has_include(<netinet/tcp.h>)
  check_boolean_sockopt<ip::tcp::no_delay>();
#endif

  check_boolean_sockopt<ip::v6_only>();

  check_integer_sockopt<ip::unicast::hops>();

  ip::multicast::join_group join(ip::address_v4::any());
  check_mcast_sockopt(join);

  ip::multicast::leave_group leave(ip::address_v4::any());
  check_mcast_sockopt(leave);

  using outbound_if = ip::multicast::outbound_interface;
  outbound_if oif(ip::address_v4::any());
  check_settable_sockopt<outbound_if, unsigned>(ip::tcp::v6(), oif);
  check_settable_sockopt<outbound_if, ::in_addr>(ip::tcp::v4(), oif);
  static_assert( is_destructible<outbound_if>(), "" );
  static_assert( ! is_default_constructible<outbound_if>(), "" );
  static_assert( is_nothrow_copy_constructible<outbound_if>(), "" );
  static_assert( is_nothrow_copy_assignable<outbound_if>(), "" );
  static_assert( is_nothrow_constructible<outbound_if, const ip::address_v4&>(), "" );
  static_assert( ! is_convertible<const ip::address_v4&, outbound_if>(), "explicit" );
  static_assert( is_nothrow_constructible<outbound_if, unsigned>(), "" );
  static_assert( ! is_convertible<unsigned, outbound_if>(), "explicit" );

  check_integer_sockopt<ip::multicast::hops>();

  check_boolean_sockopt<ip::multicast::enable_loopback>();
#endif
}

int main()
{
  test_option_types();
}
