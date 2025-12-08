// { dg-do compile { target c++26 } }
#include <mdspan>

#include <vector>

template<typename Layout, typename... Slices>
  constexpr bool
  check_slice_range(Slices... slices)
  {
    auto m = typename Layout::mapping<std::extents<int, 3, 5, 7>>{};
    auto storage = std::vector<double>(m.required_span_size());
    auto md = std::mdspan(storage.data(), m);

    auto submd = submdspan(md, slices...);           // { dg-error "expansion of" }
    (void) submd;
    return true;
  }

template<typename Layout>
  constexpr bool
  test_int_under()
  {
    check_slice_range<Layout>(1, -1, 2);             // { dg-error "expansion of" }
    return true;
  }
static_assert(test_int_under<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_int_under<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_int_under<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_int_over()
  {
    check_slice_range<Layout>(1, 5, 2);              // { dg-error "expansion of" }
    return true;
  }
static_assert(test_int_over<std::layout_left>());    // { dg-error "expansion of" }
static_assert(test_int_over<std::layout_right>());   // { dg-error "expansion of" }
static_assert(test_int_over<std::layout_stride>());  // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_tuple_under()
  {
    check_slice_range<Layout>(1, std::tuple{-1, 2}, 2);  // { dg-error "expansion of" }
    return true;
  }
static_assert(test_tuple_under<std::layout_left>());     // { dg-error "expansion of" }
static_assert(test_tuple_under<std::layout_right>());    // { dg-error "expansion of" }
static_assert(test_tuple_under<std::layout_stride>());   // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_tuple_reversed()
  {
    check_slice_range<Layout>(1, std::tuple{3, 2}, 2);   // { dg-error "expansion of" }
    return true;
  }
static_assert(test_tuple_reversed<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_tuple_reversed<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_tuple_reversed<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_tuple_over()
  {
    check_slice_range<Layout>(1, std::tuple{0, 6}, 2); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_tuple_over<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_tuple_over<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_tuple_over<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_strided_slice_zero()
  {
    check_slice_range<Layout>(1, std::strided_slice{1, 1, 0}, 2);  // { dg-error "expansion of" }
    return true;
  }
static_assert(test_strided_slice_zero<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_strided_slice_zero<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_strided_slice_zero<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_strided_slice_offset_under()
  {
    check_slice_range<Layout>(1, std::strided_slice{-1, 1, 1}, 2);   // { dg-error "expansion of" }
    return true;
  }
static_assert(test_strided_slice_offset_under<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_strided_slice_offset_under<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_strided_slice_offset_under<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_strided_slice_offset_over()
  {
    check_slice_range<Layout>(1, std::strided_slice{6, 0, 1}, 2);    // { dg-error "expansion of" }
    return true;
  }
static_assert(test_strided_slice_offset_over<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_strided_slice_offset_over<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_strided_slice_offset_over<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_strided_slice_extent_over()
  {
    check_slice_range<Layout>(1, std::strided_slice{1, 5, 1}, 2);    // { dg-error "expansion of" }
    return true;
  }
static_assert(test_strided_slice_extent_over<std::layout_left>());   // { dg-error "expansion of" }
static_assert(test_strided_slice_extent_over<std::layout_right>());  // { dg-error "expansion of" }
static_assert(test_strided_slice_extent_over<std::layout_stride>()); // { dg-error "expansion of" }

// { dg-prune-output "static assertion failed" }
// { dg-prune-output "__glibcxx_assert_fail" }
// { dg-prune-output "non-constant condition" }
// { dg-prune-output "no matching function" }
// { dg-prune-output "does not satisfy placeholder constraints" }
