// { dg-do run { target c++14 } }
// { dg-require-effective-target net_ts_ip }
// { dg-add-options net_ts }

#include <experimental/internet>
#include <cstring>
#include <testsuite_hooks.h>

using namespace std::experimental::net;

void
test_extensible()
{
#if __cplusplus >= 202002L
  static_assert(ip::tcp::endpoint().capacity() == sizeof(sockaddr_in6),
		"ip::tcp::endpoint::capacity() can store a sockaddr_in6");
#else
  VERIFY( ip::tcp::endpoint().capacity() == sizeof(sockaddr_in6) );
#endif

  ip::tcp::endpoint t1(ip::tcp::v4(), 22);
  VERIFY(t1.size() == sizeof(sockaddr_in));
  t1.resize(sizeof(sockaddr_in));
  try {
    t1.resize(2 * sizeof(sockaddr_in));
    VERIFY(false);
  } catch (const std::length_error&) {
  }

  ip::tcp::endpoint t2(ip::tcp::v6(), 80);
  VERIFY(t2.size() == sizeof(sockaddr_in6));
  t2.resize(sizeof(sockaddr_in6));
  try {
    t2.resize(2 * sizeof(sockaddr_in6));
    VERIFY(false);
  } catch (const std::length_error&) {
  }

  ip::tcp::endpoint t3;
  std::memcpy(t3.data(), t1.data(), t1.size());
  t3.resize(t1.size());
  VERIFY( t3 == t1 );
  std::memcpy(t3.data(), t2.data(), t2.size());
  t3.resize(t2.size());
  VERIFY( t3 == t2 );
}

int main()
{
  test_extensible();
}
