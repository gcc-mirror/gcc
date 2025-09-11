// { dg-do run { target c++26 } }
#include <type_traits>
#include <utility>

#include <testsuite_hooks.h>

namespace free_ops
{
  template<int OpId>
    struct UnaryOps
    {
      friend constexpr int
      operator+(UnaryOps) noexcept requires (OpId == 0)
      { return OpId; }

      friend constexpr int
      operator-(UnaryOps) noexcept requires (OpId == 1)
      { return OpId; }

      friend constexpr int
      operator~(UnaryOps) noexcept requires (OpId == 2)
      { return OpId; }

      friend constexpr int
      operator!(UnaryOps) noexcept requires (OpId == 3)
      { return OpId; }

      friend constexpr int
      operator&(UnaryOps) noexcept requires (OpId == 4)
      { return OpId; }

      friend constexpr int
      operator*(UnaryOps) noexcept requires (OpId == 5)
      { return OpId; }

      friend constexpr int
      operator++(UnaryOps) noexcept requires (OpId == 6)
      { return OpId; }

      friend constexpr int
      operator++(UnaryOps, int) noexcept requires (OpId == 7)
      { return OpId; }

      friend constexpr int
      operator--(UnaryOps) noexcept requires (OpId == 8)
      { return OpId; }

      friend constexpr int
      operator--(UnaryOps, int) noexcept requires (OpId == 9)
      { return OpId; }
    };
}

namespace member_ops
{
  template<int OpId>
    struct UnaryOps
    {
      constexpr int
      operator+() const noexcept requires (OpId == 0)
      { return OpId; }

      constexpr int
      operator-() const noexcept requires (OpId == 1)
      { return OpId; }

      constexpr int
      operator~() const noexcept requires (OpId == 2)
      { return OpId; }

      constexpr int
      operator!() const noexcept requires (OpId == 3)
      { return OpId; }

      constexpr int
      operator&() const noexcept requires (OpId == 4)
      { return OpId; }

      constexpr int
      operator*() const noexcept requires (OpId == 5)
      { return OpId; }

      constexpr int
      operator++() const noexcept requires (OpId == 6)
      { return OpId; }

      constexpr int
      operator++(int) const noexcept requires (OpId == 7)
      { return OpId; }

      constexpr int
      operator--() const noexcept requires (OpId == 8)
      { return OpId; }

      constexpr int
      operator--(int) const noexcept requires (OpId == 9)
      { return OpId; }
    };
}

constexpr size_t n_unary_ops = 10;

template<template<int> typename Ops, int OpId>
  constexpr void
  test_unary_operator()
  {
    auto x = std::cw<Ops<OpId>{}>;

    auto check = [](auto c)
    {
      VERIFY(c == OpId);
      static_assert(std::same_as<decltype(c), std::constant_wrapper<OpId>>);
    };

    if constexpr (OpId == 0)
      check(+x);
    if constexpr (OpId == 1)
      check(-x);
    if constexpr (OpId == 2)
      check(~x);
    if constexpr (OpId == 3)
      check(!x);
    if constexpr (OpId == 4)
      check(&x);
    if constexpr (OpId == 5)
      check(*x);
    if constexpr (OpId == 6)
      check(++x);
    if constexpr (OpId == 7)
      check(x++);
    if constexpr (OpId == 8)
      check(--x);
    if constexpr (OpId == 9)
      check(x--);

    static_assert(n_unary_ops == 10);
  }

constexpr void
test_unary_operators_all()
{
  auto run = []<size_t... Idx>(std::integer_sequence<size_t, Idx...>)
  {
    (test_unary_operator<free_ops::UnaryOps, Idx>(), ...);
    (test_unary_operator<member_ops::UnaryOps, Idx>(), ...);
  };
  run(std::make_index_sequence<n_unary_ops>());
}

namespace free_ops
{
  template<int OpId>
    struct BinaryOps
    {
      friend constexpr int
      operator+(BinaryOps, BinaryOps) noexcept requires (OpId == 0)
      { return OpId; }

      friend constexpr int
      operator-(BinaryOps, BinaryOps) noexcept requires (OpId == 1)
      { return OpId; }

      friend constexpr int
      operator*(BinaryOps, BinaryOps) noexcept requires (OpId == 2)
      { return OpId; }

      friend constexpr int
      operator/(BinaryOps, BinaryOps) noexcept requires (OpId == 3)
      { return OpId; }

      friend constexpr int
      operator%(BinaryOps, BinaryOps) noexcept requires (OpId == 4)
      { return OpId; }

      friend constexpr int
      operator<<(BinaryOps, BinaryOps) noexcept requires (OpId == 5)
      { return OpId; }

      friend constexpr int
      operator>>(BinaryOps, BinaryOps) noexcept requires (OpId == 6)
      { return OpId; }

      friend constexpr int
      operator&(BinaryOps, BinaryOps) noexcept requires (OpId == 7)
      { return OpId; }

      friend constexpr int
      operator|(BinaryOps, BinaryOps) noexcept requires (OpId == 8)
      { return OpId; }

      friend constexpr int
      operator^(BinaryOps, BinaryOps) noexcept requires (OpId == 9)
      { return OpId; }

      friend constexpr int
      operator&&(BinaryOps, BinaryOps) noexcept requires (OpId == 10)
      { return OpId; }

      friend constexpr int
      operator||(BinaryOps, BinaryOps) noexcept requires (OpId == 11)
      { return OpId; }

      friend constexpr int
      operator<=>(BinaryOps, BinaryOps) noexcept requires (OpId == 12)
      { return OpId; }

      friend constexpr int
      operator<(BinaryOps, BinaryOps) noexcept requires (OpId == 13)
      { return OpId; }

      friend constexpr int
      operator<=(BinaryOps, BinaryOps) noexcept requires (OpId == 14)
      { return OpId; }

      friend constexpr int
      operator==(BinaryOps, BinaryOps) noexcept requires (OpId == 15)
      { return OpId; }

      friend constexpr int
      operator!=(BinaryOps, BinaryOps) noexcept requires (OpId == 16)
      { return OpId; }

      friend constexpr int
      operator>(BinaryOps, BinaryOps) noexcept requires (OpId == 17)
      { return OpId; }

      friend constexpr int
      operator>=(BinaryOps, BinaryOps) noexcept requires (OpId == 18)
      { return OpId; }

      friend constexpr int
      operator+=(BinaryOps, BinaryOps) noexcept requires (OpId == 19)
      { return OpId; }

      friend constexpr int
      operator-=(BinaryOps, BinaryOps) noexcept requires (OpId == 20)
      { return OpId; }

      friend constexpr int
      operator*=(BinaryOps, BinaryOps) noexcept requires (OpId == 21)
      { return OpId; }

      friend constexpr int
      operator/=(BinaryOps, BinaryOps) noexcept requires (OpId == 22)
      { return OpId; }

      friend constexpr int
      operator%=(BinaryOps, BinaryOps) noexcept requires (OpId == 23)
      { return OpId; }

      friend constexpr int
      operator&=(BinaryOps, BinaryOps) noexcept requires (OpId == 24)
      { return OpId; }

      friend constexpr int
      operator|=(BinaryOps, BinaryOps) noexcept requires (OpId == 25)
      { return OpId; }
      friend constexpr int

      operator^=(BinaryOps, BinaryOps) noexcept requires (OpId == 26)
      { return OpId; }

      friend constexpr int
      operator<<=(BinaryOps, BinaryOps) noexcept requires (OpId == 27)
      { return OpId; }

      friend constexpr int
      operator>>=(BinaryOps, BinaryOps) noexcept requires (OpId == 28)
      { return OpId; }
    };
}

namespace member_ops
{
  template<int OpId>
    struct BinaryOps
    {
      constexpr int
      operator+(BinaryOps) const noexcept requires (OpId == 0)
      { return OpId; }

      constexpr int
      operator-(BinaryOps) const noexcept requires (OpId == 1)
      { return OpId; }

      constexpr int
      operator*(BinaryOps) const noexcept requires (OpId == 2)
      { return OpId; }

      constexpr int
      operator/(BinaryOps) const noexcept requires (OpId == 3)
      { return OpId; }

      constexpr int
      operator%(BinaryOps) const noexcept requires (OpId == 4)
      { return OpId; }

      constexpr int
      operator<<(BinaryOps) const noexcept requires (OpId == 5)
      { return OpId; }

      constexpr int
      operator>>(BinaryOps) const noexcept requires (OpId == 6)
      { return OpId; }

      constexpr int
      operator&(BinaryOps) const noexcept requires (OpId == 7)
      { return OpId; }

      constexpr int
      operator|(BinaryOps) const noexcept requires (OpId == 8)
      { return OpId; }

      constexpr int
      operator^(BinaryOps) const noexcept requires (OpId == 9)
      { return OpId; }

      constexpr int
      operator&&(BinaryOps) const noexcept requires (OpId == 10)
      { return OpId; }

      constexpr int
      operator||(BinaryOps) const noexcept requires (OpId == 11)
      { return OpId; }

      constexpr int
      operator<=>(BinaryOps) const noexcept requires (OpId == 12)
      { return OpId; }

      constexpr int
      operator<(BinaryOps) const noexcept requires (OpId == 13)
      { return OpId; }

      constexpr int
      operator<=(BinaryOps) const noexcept requires (OpId == 14)
      { return OpId; }

      constexpr int
      operator==(BinaryOps) const noexcept requires (OpId == 15)
      { return OpId; }

      constexpr int
      operator!=(BinaryOps) const noexcept requires (OpId == 16)
      { return OpId; }

      constexpr int
      operator>(BinaryOps) const noexcept requires (OpId == 17)
      { return OpId; }

      constexpr int
      operator>=(BinaryOps) const noexcept requires (OpId == 18)
      { return OpId; }

      constexpr int
      operator+=(BinaryOps) const noexcept requires (OpId == 19)
      { return OpId; }

      constexpr int
      operator-=(BinaryOps) const noexcept requires (OpId == 20)
      { return OpId; }

      constexpr int
      operator*=(BinaryOps) const noexcept requires (OpId == 21)
      { return OpId; }

      constexpr int
      operator/=(BinaryOps) const noexcept requires (OpId == 22)
      { return OpId; }

      constexpr int
      operator%=(BinaryOps) const noexcept requires (OpId == 23)
      { return OpId; }

      constexpr int
      operator&=(BinaryOps) const noexcept requires (OpId == 24)
      { return OpId; }

      constexpr int
      operator|=(BinaryOps) const noexcept requires (OpId == 25)
      { return OpId; }

      constexpr int
      operator^=(BinaryOps) const noexcept requires (OpId == 26)
      { return OpId; }

      constexpr int
      operator<<=(BinaryOps) const noexcept requires (OpId == 27)
      { return OpId; }

      constexpr int
      operator>>=(BinaryOps) const noexcept requires (OpId == 28)
      { return OpId; }
    };
}

constexpr size_t n_binary_ops = 29;

template<template<int> typename Ops, int OpId>
  constexpr void
  test_binary_operator()
  {
    auto cx = std::cw<Ops<OpId>{}>;
    auto cy = std::cw<Ops<OpId>{}>;

    auto check = [](auto c)
    {
      VERIFY(c == OpId);
      static_assert(std::same_as<decltype(c), std::constant_wrapper<OpId>>);
    };

    if constexpr (OpId == 0)
      check(cx + cy);
    if constexpr (OpId == 1)
      check(cx - cy);
    if constexpr (OpId == 2)
      check(cx * cy);
    if constexpr (OpId == 3)
      check(cx / cy);
    if constexpr (OpId == 4)
      check(cx % cy);
    if constexpr (OpId == 5)
      check(cx << cy);
    if constexpr (OpId == 6)
      check(cx >> cy);
    if constexpr (OpId == 7)
      check(cx & cy);
    if constexpr (OpId == 8)
      check(cx | cy);
    if constexpr (OpId == 10)
      check(cx && cy);
    if constexpr (OpId == 11)
      check(cx || cy);
    if constexpr (OpId == 12)
      check(cx <=> cy);
    if constexpr (OpId == 13)
      check(cx < cy);
    if constexpr (OpId == 14)
      check(cx <= cy);
    if constexpr (OpId == 15)
      check(cx == cy);
    if constexpr (OpId == 16)
      check(cx != cy);
    if constexpr (OpId == 17)
      check(cx > cy);
    if constexpr (OpId == 18)
      check(cx >= cy);
    if constexpr (OpId == 19)
      check(cx += cy);
    if constexpr (OpId == 20)
      check(cx -= cy);
    if constexpr (OpId == 21)
      check(cx *= cy);
    if constexpr (OpId == 22)
      check(cx /= cy);
    if constexpr (OpId == 23)
      check(cx %= cy);
    if constexpr (OpId == 24)
      check(cx &= cy);
    if constexpr (OpId == 25)
      check(cx |= cy);
    if constexpr (OpId == 26)
      check(cx ^= cy);
    if constexpr (OpId == 27)
      check(cx <<= cy);
    if constexpr (OpId == 28)
      check(cx >>= cy);
    static_assert(n_binary_ops == 29);
  }

template<template<int> typename Ops, int OpId>
  constexpr void
  test_mixed_binary_operators()
  {
    constexpr auto x = Ops<OpId>{};
    auto cx = std::cw<x>;
    constexpr auto y = Ops<OpId>{};
    auto cy = std::cw<y>;

    auto check = [](auto vc, auto cv)
    {
      auto impl = [](auto c)
      {
	VERIFY(c == OpId);
	static_assert(std::same_as<decltype(c), int>);
      };

      impl(vc);
      impl(cv);
    };

    if constexpr (OpId == 0)
      check(x + cy, cx + y);
    if constexpr (OpId == 1)
      check(x - cy, cx - y);
    if constexpr (OpId == 2)
      check(x * cy, cx * y);
    if constexpr (OpId == 3)
      check(x / cy, cx / y);
    if constexpr (OpId == 4)
      check(x % cy, cx % y);
    if constexpr (OpId == 5)
      check(x << cy, cx << y);
    if constexpr (OpId == 6)
      check(x >> cy, cx >> y);
    if constexpr (OpId == 7)
      check(x & cy, cx & y);
    if constexpr (OpId == 8)
      check(x | cy, cx | y);
    if constexpr (OpId == 10)
      check(x && cy, cx && y);
    if constexpr (OpId == 11)
      check(x || cy, cx || y);
    if constexpr (OpId == 12)
      check(x <=> cy, cx <=> y);
    if constexpr (OpId == 13)
      check(x < cy, cx < y);
    if constexpr (OpId == 14)
      check(x <= cy, cx <= y);
    if constexpr (OpId == 15)
      check(x == cy, cx == y);
    if constexpr (OpId == 16)
      check(x != cy, cx != y);
    if constexpr (OpId == 17)
      check(x > cy, cx > y);
    if constexpr (OpId == 18)
      check(x >= cy, cx >= y);
    if constexpr (OpId == 19)
      check(x += cy, cx += y);
    if constexpr (OpId == 20)
      check(x -= cy, cx -= y);
    if constexpr (OpId == 21)
      check(x *= cy, cx *= y);
    if constexpr (OpId == 22)
      check(x /= cy, cx /= y);
    if constexpr (OpId == 23)
      check(x %= cy, cx %= y);
    if constexpr (OpId == 24)
      check(x &= cy, cx &= y);
    if constexpr (OpId == 25)
      check(x |= cy, cx |= y);
    if constexpr (OpId == 26)
      check(x ^= cy, cx ^= y);
    if constexpr (OpId == 27)
      check(x <<= cy, cx <<= y);
    if constexpr (OpId == 28)
      check(x >>= cy, cx >>= y);
    static_assert(n_binary_ops == 29);
  }

constexpr void
test_binary_operators_all()
{
  auto run = []<size_t... Idx>(std::integer_sequence<size_t, Idx...>)
  {
    (test_binary_operator<free_ops::BinaryOps, Idx>(), ...);
    (test_mixed_binary_operators<free_ops::BinaryOps, Idx>(), ...);
    (test_binary_operator<member_ops::BinaryOps, Idx>(), ...);
  };
  run(std::make_index_sequence<n_binary_ops>());
}

constexpr bool
test_all()
{
  test_unary_operators_all();
  test_binary_operators_all();
  return true;
}

int
main()
{
  test_all();
  return 0;
}
