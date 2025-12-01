// { dg-do compile { target c++26 } }
#include <mdspan>

#include <cstdint>

struct NotASlice
{ };

constexpr bool
test_unrelated_stride_type()
{
  auto exts = std::extents(3, 5, 7);
  auto sub_exts = submdspan_extents(exts, 1, NotASlice{}, 2);  // { dg-error "required from" }
  return true;
}
static_assert(test_unrelated_stride_type());

constexpr bool
test_invalid_stride_zero()
{
  auto exts = std::extents(3, 5, 7);
  auto s = std::strided_slice{0, 1, 0};
  auto sub_exts = submdspan_extents(exts, 1, s, 2);  // { dg-error "expansion of" }
  return true;
}
static_assert(test_invalid_stride_zero());

template<typename Slice>
constexpr bool
test_out_of_bounds(const Slice& slice)
{
  auto exts = std::extents<uint16_t, 3, 5, 7>{};
  auto sub_exts = submdspan_extents(exts, 1, slice, 2);  // { dg-error "expansion of" }
  return true;
}
static_assert(test_out_of_bounds(std::strided_slice{0, 6, 1}));  // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::strided_slice{0, 7, 2}));  // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::strided_slice{1, 6, 1}));  // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::strided_slice{1, 6, 2}));  // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::tuple{1, 6}));             // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::tuple{std::cw<1>, std::cw<6>})); // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::strided_slice{-1, 2, 1})); // { dg-error "expansion of" }
static_assert(test_out_of_bounds(std::tuple{-1, 2}));            // { dg-error "expansion of" }

// { dg-prune-output "cannot decompose class type 'NotASlice'" }
// { dg-prune-output "static assertion failed" }
// { dg-prune-output "__glibcxx_assert_fail" }
// { dg-prune-output "non-constant condition" }
