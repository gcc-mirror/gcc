// { dg-do compile { target c++20 } }

#include <ranges>

// Bug libstdc++/111948 - subrange modifies a const size object

constexpr auto r = std::ranges::subrange(std::views::iota(0), 5);
static_assert(std::ranges::distance(r));
