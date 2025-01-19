// { dg-do compile { target c++20 } }

#include <algorithm>
#include <concepts>

struct Comp
{
  constexpr bool operator()(const int&& x, const int&& y) { return x < y; }
};

struct Proj
{
  constexpr const int&& operator()(const int& x) const { return std::move(x); }
};

static_assert(std::indirect_strict_weak_order<Comp, std::projected<const int*, Proj>>);

static_assert(std::ranges::clamp(+1, 0, 2, Comp{}, Proj{}) == 1);
static_assert(std::ranges::clamp(-1, 0, 2, Comp{}, Proj{}) == 0);
static_assert(std::ranges::clamp(10, 0, 2, Comp{}, Proj{}) == 2);


// Testcase from PR118185

struct Comp2
{
  constexpr bool operator()(const int&& x, const int&& y) const { return x < y; }
  constexpr bool operator()(const int&& x, int& y) const { return x < y; }
  constexpr bool operator()(int& x, const int&& y) const { return x < y; }
  constexpr bool operator()(int& x, int& y) const { return x < y; }
  constexpr bool operator()(std::same_as<const int&> auto && x, std::same_as<const int&> auto && y) const
  {
    return x < y;
  }
};

static_assert(std::indirect_strict_weak_order<Comp2, std::projected<const int*, Proj>>);

static_assert(std::ranges::clamp(+1, 0, 2, Comp2{}, Proj{}) == 1);
static_assert(std::ranges::clamp(-1, 0, 2, Comp2{}, Proj{}) == 0);
static_assert(std::ranges::clamp(10, 0, 2, Comp2{}, Proj{}) == 2);
