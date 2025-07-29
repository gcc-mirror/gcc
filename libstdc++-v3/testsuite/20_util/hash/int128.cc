// { dg-do run { target c++11 } }
// { dg-add-options strict_std }

#include <functional>
#include <testsuite_hooks.h>

int main()
{
#ifdef __SIZEOF_INT128__
  std::hash<__int128> h;
  __int128 i = (__int128)0x123456789;
  VERIFY( h(i) == (std::size_t)i );
  VERIFY( h(-i) == (std::size_t)-i );
  VERIFY( h(~i) == (std::size_t)~i );
  std::hash<unsigned __int128> hu;
  unsigned __int128 u = i;
  VERIFY( hu(u) == (std::size_t)u );
  VERIFY( hu(~u) == (std::size_t)~u );
#endif
}
