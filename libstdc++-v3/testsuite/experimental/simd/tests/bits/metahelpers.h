// Copyright (C) 2020-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef VC_TESTS_METAHELPERS_H_
#define VC_TESTS_METAHELPERS_H_

#include <functional>
#include <type_traits>
#include <utility>

namespace vir
{
  namespace test
  {
    template <class A, class B, class Op>
      constexpr bool
      operator_is_substitution_failure_impl(float)
      { return true; }

    template <class A, class B, class Op>
      constexpr typename std::conditional<true, bool, decltype(
	  Op()(std::declval<A>(), std::declval<B>()))>::type
      operator_is_substitution_failure_impl(int)
      { return false; }

    template <class... Ts>
      constexpr bool
      operator_is_substitution_failure()
      { return operator_is_substitution_failure_impl<Ts...>(int()); }

    template <class... Args, class F>
      constexpr auto
      sfinae_is_callable_impl(int, F &&f) -> typename std::conditional<
	true, std::true_type,
	decltype(std::forward<F>(f)(std::declval<Args>()...))>::type;

    template <class... Args, class F>
      constexpr std::false_type
      sfinae_is_callable_impl(float, const F &);

    template <class... Args, class F>
      constexpr bool
      sfinae_is_callable(F &&)
      {
	return decltype(
	    sfinae_is_callable_impl<Args...>(int(), std::declval<F>()))::value;
      }

    template <class... Args, class F>
      constexpr auto sfinae_is_callable_t(F &&f)
	-> decltype(sfinae_is_callable_impl<Args...>(int(), std::declval<F>()));

    template <class A, class B>
      constexpr bool
      has_less_bits()
      { return std::__digits_v<A> < std::__digits_v<B>; }

  }  // namespace test
}  // namespace vir

struct assignment
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() = std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) = std::forward<B>(b)))
    { return std::forward<A>(a) = std::forward<B>(b); }
};

struct bit_shift_left
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() << std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) << std::forward<B>(b)))
    { return std::forward<A>(a) << std::forward<B>(b); }
};

struct bit_shift_right
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() >> std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) >> std::forward<B>(b)))
    { return std::forward<A>(a) >> std::forward<B>(b); }
};

struct assign_modulus
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() %= std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) %= std::forward<B>(b)))
    { return std::forward<A>(a) %= std::forward<B>(b); }
};

struct assign_bit_and
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() &= std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) &= std::forward<B>(b)))
    { return std::forward<A>(a) &= std::forward<B>(b); }
};

struct assign_bit_or
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() |= std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) |= std::forward<B>(b)))
    { return std::forward<A>(a) |= std::forward<B>(b); }
};

struct assign_bit_xor
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() ^= std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) ^= std::forward<B>(b)))
    { return std::forward<A>(a) ^= std::forward<B>(b); }
};

struct assign_bit_shift_left
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() <<= std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) <<= std::forward<B>(b)))
    { return std::forward<A>(a) <<= std::forward<B>(b); }
};

struct assign_bit_shift_right
{
  template <class A, class B>
    constexpr decltype(std::declval<A>() >>= std::declval<B>())
    operator()(A &&a, B &&b) const noexcept(noexcept(
	std::forward<A>(a) >>= std::forward<B>(b)))
    { return std::forward<A>(a) >>= std::forward<B>(b); }
};

template <class A, class B, class Op = std::plus<>>
  constexpr bool is_substitution_failure
    = vir::test::operator_is_substitution_failure<A, B, Op>();

using vir::test::sfinae_is_callable;

using vir::test::has_less_bits;

#endif  // VC_TESTS_METAHELPERS_H_
