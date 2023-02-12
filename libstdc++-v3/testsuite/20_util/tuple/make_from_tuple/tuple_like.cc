// { dg-do compile { target c++17 } }

#include <tuple>
#include <utility>

struct Two
{
  Two(const char*, int);
};

void
test_pair()
{
  auto two = std::make_from_tuple<Two>(std::pair("one", 2));
  static_assert(std::is_same_v<decltype(two), Two>, "make from pair");
}

#include <array>

struct Three
{
  Three(int, int, int);
};

void
test_array()
{
  Three three = std::make_from_tuple<Three>(std::array<int, 3>{{1, 2, 3}});
  static_assert(std::is_same_v<decltype(three), Three>, "make from array");
}

#if __cplusplus >= 202002L
#include <vector>
#include <ranges>

void
test_subrange() // PR libstdc++/102301
{
  auto r = std::views::iota(0, 5);
  auto v = std::make_from_tuple<std::vector<int>>(std::ranges::subrange(r));
  static_assert(std::is_same_v<decltype(v), std::vector<int>>, "from subrange");
}
#endif
