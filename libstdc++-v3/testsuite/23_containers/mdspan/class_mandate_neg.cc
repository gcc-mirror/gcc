// { dg-do compile { target c++23 } }
#include<mdspan>

#include <cstdint>
#include "layout_like.h"

struct ExtentsLike
{
  using index_type = int;
  using size_type = unsigned int;
  using rank_type = size_t;

  static constexpr size_t rank() { return 1; }
  static constexpr size_t rank_dynamic() { return 0; }
};

constexpr bool
test_custom_extents_type()
{
  std::mdspan<double, ExtentsLike> md1; // { dg-error "required from here" }
  return true;
}
static_assert(test_custom_extents_type());

constexpr bool
test_element_type_mismatch()
{
  using E = std::extents<int, 1>;
  using L = std::layout_right;
  using A = std::default_accessor<double>;

  [[maybe_unused]] std::mdspan<float, E, L, A> md2; // { dg-error "required from here" }
  return true;
};
static_assert(test_element_type_mismatch());

// { dg-prune-output "Extents must be a specialization of std::extents" }
// { dg-prune-output "no type named '_Storage'" }
// { dg-prune-output "non-constant condition" }
// { dg-prune-output "static assertion failed" }
// { dg-prune-output "__glibcxx_assert" }
