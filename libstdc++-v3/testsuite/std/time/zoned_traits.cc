// { dg-do run { target c++20 } }
// { dg-require-effective-target cxx11_abi }

#include <chrono>
#include <testsuite_hooks.h>

using namespace std::chrono;

static_assert( std::is_empty_v<zoned_traits<const time_zone*>> );
static_assert(std::is_default_constructible_v<zoned_traits<const time_zone*>>);

// The primary template is a complete type, it just has no members.
static_assert( std::is_empty_v<zoned_traits<time_zone*>> );
static_assert(std::is_default_constructible_v<zoned_traits<time_zone*>>);
static_assert( std::is_empty_v<zoned_traits<int>> );
static_assert(std::is_default_constructible_v<zoned_traits<int>>);

void
test_default_zone()
{
  auto p = zoned_traits<const time_zone*>::default_zone();
  static_assert( std::is_same_v<decltype(p), const time_zone*> );
  VERIFY( p == locate_zone("UTC") );
}

void
test_locate_zone()
{
  auto p = zoned_traits<const time_zone*>::locate_zone("GMT");
  static_assert( std::is_same_v<decltype(p), const time_zone*> );
  VERIFY( p == locate_zone("GMT") );
}

int main()
{
  test_default_zone();
  test_locate_zone();
}
