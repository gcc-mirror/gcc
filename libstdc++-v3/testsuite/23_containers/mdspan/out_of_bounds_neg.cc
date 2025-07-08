// { dg-do compile { target c++23 } }
#include<mdspan>

#include "layout_like.h"

template<typename Layout>
constexpr bool
test_invalid_multi_index()
{

  double data = 1.1;
  auto m = typename Layout::mapping<std::extents<int, 1, 2, 3>>{};
  auto md = std::mdspan(&data, m);

  [[maybe_unused]] double x = md[0, 2, 2]; // { dg-error "expansion of" }
  return true;
};
static_assert(test_invalid_multi_index<LayoutLike>()); // { dg-error "expansion of" }
static_assert(test_invalid_multi_index<std::layout_left>()); // { dg-error "expansion of" }
static_assert(test_invalid_multi_index<std::layout_right>()); // { dg-error "expansion of" }
static_assert(test_invalid_multi_index<std::layout_stride>()); // { dg-error "expansion of" }

// { dg-prune-output "non-constant condition" }
// { dg-prune-output "__glibcxx_assert" }
