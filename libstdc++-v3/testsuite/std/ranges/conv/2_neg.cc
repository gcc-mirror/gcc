// { dg-do compile { target c++23 } }

// C++23 26.5.7 Range conversions [range.utility.conv]

#include <ranges>
#include <vector>
#include <testsuite_allocator.h>

void
test_2_1_5()
{
  // (2.1.5) Otherwise, the program is ill-formed.

  using Alloc = __gnu_test::uneq_allocator<int>;
  using Vec = std::vector<int, Alloc>;

  std::vector<int> v;
  (void) std::ranges::to<Vec>(v, v.get_allocator()); // { dg-error "here" }

  (void) std::ranges::to<Vec>(Vec{}, 1, 2, 3, 4, 5, 6); // { dg-error "here" }
}

// { dg-error "static assertion failed" "" { target *-*-* } 0 }
// { dg-prune-output "no matching function" }
