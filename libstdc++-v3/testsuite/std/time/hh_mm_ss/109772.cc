// { dg-do compile { target c++20 } }

// PR libstdc++/109772 Memory layout optimization of chrono::hh_mm_ss is wrong

#include <chrono>
#include <testsuite_hooks.h>

using std::chrono::hh_mm_ss;
using std::chrono::duration;
using std::ratio;

constexpr bool
test_truncated()
{
  using int_1_1024 = duration<int, ratio<1, 1024>>;
  static_assert( hh_mm_ss<int_1_1024>::fractional_width == 10 );

  hh_mm_ss<int_1_1024> t1(int_1_1024(487));
  VERIFY( t1.subseconds().count() == (10'000'000'000 * 487 / 1024) );

  hh_mm_ss<int_1_1024> t2(int_1_1024(1023));
  VERIFY( t2.subseconds().count() == (10'000'000'000 * 1023 / 1024) );

  return true;
}

static_assert( test_truncated() );

// Check for ambiguous partial specializations of hh_mm_ss::__subseconds:
static_assert( hh_mm_ss<duration<char, ratio<1, 11>>>::fractional_width == 6 );
