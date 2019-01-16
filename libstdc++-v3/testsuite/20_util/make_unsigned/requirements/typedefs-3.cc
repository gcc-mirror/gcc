// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2019 Free Software Foundation, Inc.
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

#include <type_traits>

template<typename T, typename I0, typename... I>
struct smallest_rank
: std::conditional< sizeof(T) == sizeof(I0),
                    I0,
                    typename smallest_rank<T, I...>::type >
{ };

template<typename T, typename I0>
struct smallest_rank<T, I0>
{ using type = I0; };

template<typename T>
using smallest_rank_t
  = typename smallest_rank<typename std::remove_cv<T>::type,
                           unsigned char, unsigned short, unsigned int,
                           unsigned long, unsigned long long>::type;

using std::make_unsigned;
using std::is_same;

enum E1 : char { };
using I1 = smallest_rank_t<E1>;
static_assert(is_same<make_unsigned<E1>::type, I1>::value, "");
static_assert(is_same<make_unsigned<E1 const>::type, I1 const>::value, "");

enum E2 : short { };
using I2 = smallest_rank_t<E2>;
static_assert(is_same<make_unsigned<E2>::type, I2>::value, "");
static_assert(is_same<make_unsigned<E2 const>::type, I2 const>::value, "");

enum E3 : int { };
using I3 = smallest_rank_t<E3>;
static_assert(is_same<make_unsigned<E3>::type, I3>::value, "");
static_assert(is_same<make_unsigned<E3 const>::type, I3 const>::value, "");

enum E4 : long { };
using I4 = smallest_rank_t<E4>;
static_assert(is_same<make_unsigned<E4>::type, I4>::value, "");
static_assert(is_same<make_unsigned<E4 const>::type, I4 const>::value, "");

// PR libstdc++/60333
enum E5 : long long { };
using I5 = smallest_rank_t<E5>;
static_assert(is_same<make_unsigned<E5>::type, I5>::value, "");
static_assert(is_same<make_unsigned<E5 const>::type, I5 const>::value, "");

// PR libstdc++/85951
using I6 = smallest_rank_t<char16_t>;
static_assert(is_same<make_unsigned<char16_t>::type, I6>::value, "");
static_assert(is_same<make_unsigned<char16_t const>::type, I6 const>::value, "");
using I7 = smallest_rank_t<char32_t>;
static_assert(is_same<make_unsigned<char32_t>::type, I7>::value, "");
static_assert(is_same<make_unsigned<char32_t const>::type, I7 const>::value, "");
#ifdef _GLIBCXX_USE_WCHAR_T
using I8 = smallest_rank_t<wchar_t>;
static_assert(is_same<make_unsigned<wchar_t>::type, I8>::value, "");
static_assert(is_same<make_unsigned<wchar_t const>::type, I8 const>::value, "");
#endif
