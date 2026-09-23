// { dg-do compile { target c++23 } }
#include <mdspan>

#include "../layout_traits.h"
#include <cstdint>

constexpr size_t dyn = std::dynamic_extent;

constexpr bool
test_stride_overflow()
{
  auto exts = std::extents<uint8_t, dyn, dyn>(1, 3);
  auto n = size_t(1) << 9;
  auto m = std::layout_stride::mapping(exts, std::array{n, 1zu}); // { dg-error "expansion of" }
  (void) m;
  return true;
}
static_assert(test_stride_overflow()); // { dg-error "expansion of" }

constexpr bool
test_stride_negative()
{
  auto exts = std::extents<std::size_t, dyn, dyn>(1, 3);
  auto m = std::layout_stride::mapping(exts, std::array{1, -4}); // { dg-error "expansion of" } 
  (void) m;
  return true;
}
static_assert(test_stride_negative()); // { dg-error "expansion of" } 

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "__glibcxx_assert_fail()" }
