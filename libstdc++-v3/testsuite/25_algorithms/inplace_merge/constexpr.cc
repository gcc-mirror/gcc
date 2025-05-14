// { dg-do compile { target c++26 } }

#include <algorithm>
#include <array>
#include <functional>
#include <utility>

// returns a pair [array, index of partitioning point]
constexpr auto
create_array()
{
  return std::make_pair(
    std::to_array({0, 2, 2, 2, 4, 6, 1, 2, 3, 3, 4, 4, 5}),
    6);
}

constexpr bool
test01()
{
  auto [ar, index] = create_array();
  std::inplace_merge(ar.begin(), ar.begin() + index, ar.end());
  return std::is_sorted(ar.begin(), ar.end());
}

static_assert(test01());

constexpr bool
test02()
{
  auto [ar, index] = create_array();
  auto index_it = ar.begin() + index;
  std::reverse(ar.begin(), index_it);
  std::reverse(index_it, ar.end());
  std::inplace_merge(ar.begin(), index_it, ar.end(), std::greater<>());
  return std::is_sorted(ar.begin(), ar.end(), std::greater<>());
}

static_assert(test02());

constexpr bool
test03()
{
  auto [ar, index] = create_array();
  std::ranges::inplace_merge(ar, ar.begin() + index);
  return std::ranges::is_sorted(ar);
}

static_assert(test03());

constexpr bool
test04()
{
  auto [ar, index] = create_array();
  auto index_it = ar.begin() + index;
  std::ranges::reverse(ar.begin(), index_it);
  std::ranges::reverse(index_it, ar.end());
  std::ranges::inplace_merge(ar, index_it, std::ranges::greater());
  return std::ranges::is_sorted(ar, std::ranges::greater());
}

static_assert(test04());

constexpr bool
test05()
{
  auto [ar, index] = create_array();
  auto proj = [](int i) { return -i; };
  std::ranges::inplace_merge(ar, ar.begin() + index, std::ranges::greater(), proj);
  return std::ranges::is_sorted(ar, std::ranges::greater(), proj);
}

static_assert(test05());
