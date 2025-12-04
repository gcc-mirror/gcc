// { dg-do compile { target c++20 } }

#include <concepts>
#include <compare>

// P2404R3 Move-only types for equality_comparable_with,
// totally_ordered_with, and three_way_comparable_with

// This was approved for C++23 but we treat it as a DR for C++20.

#ifndef __cpp_lib_concepts
# error "Feature-test macro __cpp_lib_concepts is missing in <compare>"
#elif __cpp_lib_concepts < 202207L
# error "Feature-test macro __cpp_lib_concepts has wrong value in <compare>"
#endif

struct MoveOnly
{
  MoveOnly(int);
  MoveOnly(MoveOnly&&) = default;
  auto operator<=>(const MoveOnly&) const = default;
  std::strong_ordering operator<=>(int) const;
  bool operator==(const MoveOnly&) const;
};

static_assert(std::equality_comparable_with<MoveOnly, int>);
static_assert(std::totally_ordered_with<MoveOnly, int>);
static_assert(std::three_way_comparable_with<MoveOnly, int>);
