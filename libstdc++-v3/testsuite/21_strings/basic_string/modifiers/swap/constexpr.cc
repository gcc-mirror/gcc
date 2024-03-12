// { dg-do compile { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <string>
#include <testsuite_hooks.h>

using C = char;
using T = std::char_traits<C>;

constexpr bool
test_swap()
{
  std::basic_string<C> s0;
  s0.swap(s0);
  VERIFY( s0.empty() );
  std::basic_string<C> s00;
  s0.swap(s00);
  VERIFY( s0.empty() );
  VERIFY( s00.empty() );

  std::basic_string<C> s1 = "s1";
  s1.swap(s0);
  VERIFY( s1.empty() );
  VERIFY( ! s0.empty() );
  s1.swap(s0);
  VERIFY( s0.empty() );
  VERIFY( ! s1.empty() );

  std::basic_string<C> s2 = "quite a long string, but not very long";
  s2.swap(s1);
  VERIFY( s2.length() == 2 );
  VERIFY( s1.length() == 38 );
  s2.swap(s1);
  VERIFY( s1.length() == 2 );
  VERIFY( s2.length() == 38 );

  swap(s2, s0);
  VERIFY( s2.empty() );
  VERIFY( s0.length() == 38 );

  auto s3 = s0;
  swap(s3, s0);
  VERIFY( s3 == s0 );

  return true;
}

static_assert( test_swap() );
