// { dg-do compile { target c++23 } }
// { dg-options "-std=c++23" }

// Test for PR libstdc++/119721: tuple<> comparison with array<T, 0>

#include <tuple>
#include <array>
#include <testsuite_hooks.h>
#include <cassert>

constexpr void
test01()
{
  std::tuple<> t;
  std::array<int, 0> a;

  // Basic comparison should work
  VERIFY( t == a );
  VERIFY( a == t );
  VERIFY( !(t != a) );
  VERIFY( !(a != t) );

  // Ordering comparisons should be equal
  VERIFY( !(t < a) );
  VERIFY( !(t > a) );
  VERIFY( t <= a );
  VERIFY( t >= a );
  VERIFY( !(a < t) );
  VERIFY( !(a > t) );
  VERIFY( a <= t );
  VERIFY( a >= t );

  // Three-way comparison should return equal
  VERIFY( (t <=> a) == std::strong_ordering::equal );
  VERIFY( (a <=> t) == std::strong_ordering::equal );
}

constexpr void
test02()
{
  // Test with non-comparable element type
  struct NonComparable {
    void operator==(const NonComparable&) const = delete;
    void operator<=>(const NonComparable&) const = delete;
  };

  std::tuple<> t;
  std::array<NonComparable, 0> a;

  // Should still work because empty containers don't compare elements
  VERIFY( t == a );
  VERIFY( (t <=> a) == std::strong_ordering::equal );
}

int main()
{
  auto test_all = [] {
    test01();
    test02();
  };

  test_all();
  static_VERIFY( test_all() );
  return 0;
}
