// { dg-options "-D_GLIBCXX_DEBUG" }
// { dg-do compile { target c++20 } }

// Bug libstdc++/106212 - Code becomes non-constexpr with _GLIBCXX_DEBUG

#include <array>

struct A
{
  constexpr A(int i) : e{i} {}
  constexpr bool operator==(const A& a) const = default;
  std::array<int, 1> e;
};

static_assert(A{1} != A{2}, "");
