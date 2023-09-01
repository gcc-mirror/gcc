// { dg-do run { target c++23 } }

#include <ranges>
#include <algorithm>
#include <testsuite_hooks.h>

namespace ranges = std::ranges;
namespace views = std::views;

struct _Negate : ranges::range_adaptor_closure<_Negate>
{
  template<ranges::viewable_range _Range>
  constexpr auto
  operator()(_Range&& __r) const
  requires requires { views::transform(std::declval<_Range>(), std::negate{}); }
  { return views::transform(std::forward<_Range>(__r), std::negate{}); }
};

constexpr _Negate negate;

constexpr bool
test01()
{
  int x[] = {1, 2, 3};
  VERIFY( ranges::equal(x | negate, (int[]){-1, -2, -3}) );
  VERIFY( ranges::equal(x | negate | negate, x) );
  VERIFY( ranges::equal(x | (negate | negate), x) );
  VERIFY( ranges::equal(x | views::reverse | negate, x | negate | views::reverse) );
  VERIFY( ranges::equal(x | (views::reverse | negate), x | (negate | views::reverse)) );
#if 0
  // These asserts currently fail for the same reason as the disabled asserts
  // in std/ranges/adaptors/all.cc.
  static_assert( sizeof(negate | views::reverse | views::join) == 1 );
  static_assert( sizeof(views::reverse | negate | views::join) == 1 );
  static_assert( sizeof(views::reverse | views::join | negate) == 1 );
#endif

  return true;
}

int
main()
{
  static_assert(test01());
}
