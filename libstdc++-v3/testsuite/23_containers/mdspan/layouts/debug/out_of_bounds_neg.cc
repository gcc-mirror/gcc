// { dg-do compile { target c++23 } }
// { dg-require-debug-mode "" }
#include<mdspan>

template<typename Layout>
  constexpr bool
  test_out_of_bounds_1d()
  {
    auto m = typename Layout::mapping<std::extents<int, 0>>{};
    (void) m(0); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_out_of_bounds_1d<std::layout_left>()); // { dg-error "expansion of" }
static_assert(test_out_of_bounds_1d<std::layout_right>()); // { dg-error "expansion of" }
static_assert(test_out_of_bounds_1d<std::layout_stride>()); // { dg-error "expansion of" }

template<typename Layout>
  constexpr bool
  test_out_of_bounds_3d()
  {
    auto m = typename Layout::mapping<std::extents<int, 3, 5, 7>>{};
    (void) m(2, 5, 5); // { dg-error "expansion of" }
    return true;
  }
static_assert(test_out_of_bounds_3d<std::layout_left>()); // { dg-error "expansion of" }
static_assert(test_out_of_bounds_3d<std::layout_right>()); // { dg-error "expansion of" }
static_assert(test_out_of_bounds_3d<std::layout_stride>()); // { dg-error "expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "__glibcxx_assert" }
