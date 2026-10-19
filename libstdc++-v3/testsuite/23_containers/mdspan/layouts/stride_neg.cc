// { dg-do compile { target c++23 } }
#include <mdspan>
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

using std::array;
template<size_t Rank>
 using mapping = std::layout_stride::mapping<std::dextents<size_t, Rank>>;

constexpr auto m0 = mapping<0>(array<int, 0>{}, array<int, 0>{}); // { dg-bogus "expansion of" }

// Zero strides allowed for empty mappings
constexpr auto m00 = mapping<1>(array{0}, array{0}); // { dg-bogus "expansion of" }
constexpr auto m01 = mapping<2>(array{0, 2}, array{3, 0}); // { dg-bogus "expansion of" }
constexpr auto m02 = mapping<2>(array{0, 2}, array{0, 0}); // { dg-bogus "expansion of" }
constexpr auto m03 = mapping<3>(array{0, 2, 3}, array{3, 0, 0}); // { dg-bogus "expansion of" }
constexpr auto m04 = mapping<3>(array{0, 2, 3}, array{3, 3, 0}); // { dg-bogus "expansion of" }
constexpr auto m05 = mapping<4>(array{0, 2, 3, 4}, array{5, 3, 0, 2}); // { dg-bogus "expansion of" }

// On dimensions with stride equal to 1
constexpr auto m0a = mapping<1>(array{1}, array{0}); // { dg-bogus "expansion of" }
constexpr auto m0b = mapping<2>(array{2, 1}, array{3, 0}); // { dg-bogus "expansion of" }
constexpr auto m0c = mapping<2>(array{1, 1}, array{0, 0}); // { dg-bogus "expansion of" }
constexpr auto m0d = mapping<3>(array{2, 1, 1}, array{3, 0, 0}); // { dg-bogus "expansion of" }
constexpr auto m0e = mapping<3>(array{2, 2, 1}, array{6, 3, 0}); // { dg-bogus "expansion of" }
constexpr auto m0f = mapping<4>(array{2, 2, 1, 2}, array{8, 4, 0, 2}); // { dg-bogus "expansion of" }

// Otherwise leads to non-unique mappings.
constexpr auto m06 = mapping<1>(array{2}, array{0}); // { dg-error "expansion of" }
constexpr auto m07 = mapping<2>(array{2, 3}, array{3, 0}); // { dg-error "expansion of" }
constexpr auto m08 = mapping<3>(array{2, 3, 4}, array{3, 3, 0}); // { dg-error "expansion of" }
constexpr auto m09 = mapping<4>(array{2, 3, 4, 5}, array{5, 3, 0, 2}); // { dg-error "expansion of" }

// required_span_size is smaller size of multidimensional index space
constexpr auto m10 = mapping<2>(array{3, 4}, array{2, 3}); // { dg-error "expansion of" }
constexpr auto m11 = mapping<2>(array{3, 4}, array{1, 2}); // { dg-error "expansion of" }
constexpr auto m12 = mapping<3>(array{3, 4, 5}, array{1, 3, 11}); // { dg-error "expansion of" }
constexpr auto m13 = mapping<3>(array{3, 4, 5}, array{1, 2, 12}); // { dg-error "expansion of" }
constexpr auto m14 = mapping<4>(array{3, 4, 5, 6}, array{1, 3, 12, 50}); // { dg-error "expansion of" }
constexpr auto m15 = mapping<4>(array{3, 4, 5, 6}, array{1, 3, 11, 60}); // { dg-error "expansion of" }
constexpr auto m16 = mapping<4>(array{3, 4, 5, 6}, array{1, 2, 12, 60}); // { dg-error "expansion of" }

// Passes required_span_size check (smaller and bigger stride), detected only in debug mode
constexpr auto m17 = mapping<3>(array{3, 4, 5}, array{1, 2, 25}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m18 = mapping<3>(array{3, 4, 5}, array{1, 5, 12}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m19 = mapping<4>(array{3, 4, 5, 6}, array{1, 2, 12, 100}); // { dg-error "expansion of" "" { target debug_mode } }
constexpr auto m20 = mapping<4>(array{3, 4, 5, 6}, array{1, 5, 12, 60});  // { dg-error "expansion of" "" { target debug_mode } }

// Any stride values are allowed for extents 1
constexpr auto m21 = mapping<2>(array{1, 1}, array{88, 99}); // { dg-bogus "expansion of" }
constexpr auto m2a = mapping<2>(array{5, 1}, array{88, 99}); // { dg-bogus "expansion of" }
constexpr auto m22 = mapping<3>(array{1, 1, 1}, array{88, 99, 00}); // { dg-bogus "expansion of" }
constexpr auto m2b = mapping<3>(array{1, 5, 1}, array{88, 99, 77}); // { dg-bogus "expansion of" }
constexpr auto m23 = mapping<4>(array{1, 1, 1, 1}, array{77, 99, 00, 88}); // { dg-bogus "expansion of" }
constexpr auto m2c = mapping<4>(array{1, 1, 5, 1}, array{77, 99, 1, 88}); // { dg-bogus "expansion of" }
constexpr auto m24 = mapping<5>(array{3, 4, 1, 1, 5}, array{3, 10, 3, 3, 45}); // { dg-bogus "expansion of" }
constexpr auto m25 = mapping<5>(array{3, 4, 1, 1, 5}, array{3, 10, 3, 10, 45}); // { dg-bogus "expansion of" }
constexpr auto m26 = mapping<5>(array{3, 4, 1, 1, 5}, array{3, 10, 10, 3, 45}); // { dg-bogus "expansion of" }
constexpr auto m27 = mapping<5>(array{3, 4, 1, 1, 5}, array{3, 10, 10, 10, 45}); // { dg-bogus "expansion of" }
constexpr auto m2d = mapping<5>(array{3, 4, 1, 1, 5}, array{3, 10, 00, 99, 45}); // { dg-bogus "expansion of" }
constexpr auto m2e = mapping<5>(array{1, 1, 1, 1, 1}, array{00, 77, 88, 99, 00}); // { dg-bogus "expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "__glibcxx_assert_fail()" }
