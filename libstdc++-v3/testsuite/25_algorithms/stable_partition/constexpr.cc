// { dg-do compile { target c++26 } }

#include <algorithm>
#include <array>

constexpr auto
create_array()
{
  return std::to_array({0, 10, 1, 2, 3, 3, 4, -1, -2, -4, 5, 6});
}

constexpr bool
test01()
{
  auto ar = create_array();
  auto pred = [](int i) { return i % 2 == 0; };
  std::stable_partition(ar.begin(), ar.end(), pred);
  return std::is_partitioned(ar.begin(), ar.end(), pred);
}

static_assert(test01());

constexpr bool
test02()
{
  auto ar = create_array();
  auto pred = [](int i) { return i % 2 == 0; };
  std::ranges::stable_partition(ar, pred);
  return std::ranges::is_partitioned(ar, pred);
}

static_assert(test02());

constexpr bool
test03()
{
  auto ar = create_array();
  auto pred = [](int i) { return i % 2 == 0; };
  auto proj = [](int i) { return i + 1; };
  std::ranges::stable_partition(ar, pred, proj);
  return std::ranges::is_partitioned(ar, pred, proj);
}

static_assert(test03());
