// Copyright (C) 2020-2023 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>

using AT = std::allocator_traits<std::allocator<int>>;

template<typename...> using void_t = void;

template<typename T, typename U, typename = void>
struct has_construct
: std::false_type
{ };

template<typename T, typename U>
struct has_construct<T, U,
    void_t<decltype(std::declval<T&>().construct(std::declval<U*>()))>>
: std::true_type
{ };

template<typename T, typename U, typename = void>
struct has_destroy
: std::false_type
{ };

template<typename T, typename U>
struct has_destroy<T, U,
    void_t<decltype(std::declval<T&>().destroy(std::declval<U*>()))>>
: std::true_type
{ };

template<typename T, typename U, typename = void>
struct has_traits_construct
: std::false_type
{ };

template<typename T, typename U>
struct has_traits_construct<T, U,
    void_t<decltype(AT::construct(std::declval<T&>(), std::declval<U*>()))>>
: std::true_type
{ };

template<typename T, typename U, typename = void>
struct has_traits_destroy
: std::false_type
{ };

template<typename T, typename U>
struct has_traits_destroy<T, U,
    void_t<decltype(AT::destroy(std::declval<T&>(), std::declval<U*>()))>>
: std::true_type
{ };

struct NoDefault { NoDefault(int); };
struct NoDest { private: ~NoDest(); };

// Whether true or false, this should not give an error:
constexpr bool c = has_construct<std::allocator<NoDefault>, NoDefault>::value;
constexpr bool cv = has_construct<std::allocator<void>, NoDefault>::value;
constexpr bool c2 = has_traits_construct<std::allocator<int>, NoDefault>::value;
constexpr bool d = has_destroy<std::allocator<NoDest>, NoDest>::value;
constexpr bool d2 = has_traits_destroy<std::allocator<int>, NoDest>::value;

std::allocator<int> a;

long* lp;
#if __cplusplus <= 201703L
static_assert( noexcept(a.construct(lp)), "" );
static_assert( noexcept(a.construct(lp, 1L)), "" );
static_assert( noexcept(a.construct(lp, 2)), "" );
static_assert( noexcept(a.construct(lp, 2U)), "" );
static_assert( noexcept(a.destroy(lp)), "" );
#endif
static_assert( noexcept(AT::construct(a, lp)), "" );
static_assert( noexcept(AT::construct(a, lp, 1L)), "" );
static_assert( noexcept(AT::construct(a, lp, 2)), "" );
static_assert( noexcept(AT::construct(a, lp, 2U)), "" );
static_assert( noexcept(AT::destroy(a, lp)), "" );

struct X
{
  X() noexcept;
  X(int) noexcept;
  ~X() noexcept;
};

X* xp;
#if __cplusplus <= 201703L
static_assert( noexcept(a.construct(xp)), "" );
static_assert( noexcept(a.construct(xp, 1)), "" );
static_assert( noexcept(a.destroy(xp)), "" );
#endif
static_assert( noexcept(AT::construct(a, xp)), "" );
static_assert( noexcept(AT::construct(a, xp, 1)), "" );
static_assert( noexcept(AT::destroy(a, xp)), "" );

struct Y
{
  Y() noexcept;
  Y(int) noexcept(false);
  ~Y() noexcept;
};

Y* yp;
#if __cplusplus <= 201703L
static_assert( noexcept(a.construct(yp)), "" );
static_assert( ! noexcept(a.construct(yp, 1)), "" );
static_assert( noexcept(a.destroy(yp)), "" );
#endif
static_assert( noexcept(AT::construct(a, yp)), "" );
static_assert( ! noexcept(AT::construct(a, yp, 1)), "" );
static_assert( noexcept(AT::destroy(a, yp)), "" );

struct Z
{
  Z() noexcept;
  Z(int) noexcept;
  ~Z() noexcept(false);
};

Z* zp;
// These construct calls should be noexcept, but they are false because
// they use is_nothrow_constructible which depends on is_nothrow_destructible.
#if __cplusplus <= 201703L
static_assert( ! noexcept(a.construct(zp)), "wrong" );
static_assert( ! noexcept(a.construct(zp, 1)), "wrong" );
static_assert( ! noexcept(a.destroy(zp)), "" );
#endif
static_assert( ! noexcept(AT::construct(a, zp)), "" );
static_assert( ! noexcept(AT::construct(a, zp, 1)), "" );
static_assert( ! noexcept(AT::destroy(a, zp)), "" );
