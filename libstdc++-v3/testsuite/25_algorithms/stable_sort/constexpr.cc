// { dg-do compile { target c++26 } }

#include <algorithm>
#include <array>
#include <functional>

constexpr auto
createArray()
{
  return std::to_array({10, 0, 1, 2, 5, 6, 7, 8, 3, 4, 9, 11});
}

constexpr bool
test01()
{
  auto ar = createArray();
  std::stable_sort(ar.begin(), ar.end());
  return std::is_sorted(ar.begin(), ar.end());
}

static_assert(test01());

constexpr bool
test02()
{
  auto ar = createArray();
  std::stable_sort(ar.begin(), ar.end(), std::greater<>());
  return std::is_sorted(ar.begin(), ar.end(), std::greater<>());
}

static_assert(test02());

constexpr bool
test03()
{
  auto ar = createArray();
  std::ranges::stable_sort(ar);
  return std::ranges::is_sorted(ar);
}

static_assert(test03());

constexpr bool
test04()
{
  auto ar = createArray();
  std::ranges::stable_sort(ar, std::ranges::greater());
  return std::ranges::is_sorted(ar, std::ranges::greater());
}

static_assert(test04());

constexpr bool
test05()
{
  auto ar = createArray();
  auto proj = [](int i) { return -i; };
  std::ranges::stable_sort(ar, {}, proj);
  return std::ranges::is_sorted(ar, {}, proj);
}

static_assert(test05());
