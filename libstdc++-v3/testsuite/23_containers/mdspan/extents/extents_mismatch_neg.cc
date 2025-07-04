// { dg-do compile { target c++23 } }
#include<mdspan>

#include <cstdint>

constexpr size_t dyn = std::dynamic_extent;

constexpr bool
test_dyn2sta_extents_mismatch_00()
{
  auto e0 = std::extents<int, dyn>{1};
  [[maybe_unused]] auto e1 = std::extents<int, 2>{e0};        // { dg-error "expansion of" }
  return true;
}
static_assert(test_dyn2sta_extents_mismatch_00());            // { dg-error "expansion of" }

constexpr bool
test_dyn2sta_extents_mismatch_01()
{
  [[maybe_unused]] auto e = std::extents<int, 1, dyn>{2, 2}; // { dg-error "expansion of" }
  return true;
}
static_assert(test_dyn2sta_extents_mismatch_01());           // { dg-error "expansion of" }

constexpr bool
test_dyn2sta_extents_mismatch_02()
{
  std::array<int, 2> exts{2, 2};
  [[maybe_unused]] auto e = std::extents<int, 1, dyn>{exts}; // { dg-error "expansion of" }
  return true;
}
static_assert(test_dyn2sta_extents_mismatch_02());           // { dg-error "expansion of" }

// { dg-prune-output "non-constant condition for static assertion" }
// { dg-prune-output "__glibcxx_assert" }
