// { dg-do run { target c++20 } }

#include <utility>
#include <limits>
#include <testsuite_hooks.h>

void
test01()
{
  unsigned int u = std::numeric_limits<unsigned int>::max();
  int s = -1;
  VERIFY( !std::cmp_greater(s, u) );
  VERIFY( std::cmp_greater(u, s) );
  u = (unsigned) std::numeric_limits<int>::max() + 1U;
  VERIFY( !std::cmp_greater(s, u) );
  VERIFY( std::cmp_greater(u, s) );
}

constexpr bool
test02()
{
  unsigned int u = std::numeric_limits<unsigned int>::max();
  int s = -1;
  if (std::cmp_greater(s, u))
    throw 1;
  if (!std::cmp_greater(u, s))
    throw 2;
  return true;
}

void
test03()
{
  short ss = -1;
  int s = -1;
  VERIFY( !std::cmp_greater(s, ss) );
  VERIFY( !std::cmp_greater(ss, s) );

  unsigned int u = (unsigned int) -1;
  VERIFY( !std::cmp_greater(s, u) );
  VERIFY( std::cmp_greater(u, s) );
  VERIFY( !std::cmp_greater(ss, u) );
  VERIFY( std::cmp_greater(u, ss) );

  unsigned long long ul = (unsigned long long) -1;
  VERIFY( !std::cmp_greater(s, ul) );
  VERIFY( std::cmp_greater(ul, s) );
  VERIFY( !std::cmp_greater(ss, ul) );
  VERIFY( std::cmp_greater(ul, ss) );
  VERIFY( !std::cmp_greater(u, ul) );
  VERIFY( std::cmp_greater(ul, u) );
}

int
main()
{
  test01();
  static_assert( test02() );
  test03();
}
