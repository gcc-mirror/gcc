// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

#include <memory>

using std::is_same;

template<typename T, typename U>
  using Rebind = typename std::pointer_traits<T>::template rebind<U>;

template<typename T>
  struct HasRebind {
    template<typename U> using rebind = U*;
  };

static_assert(is_same<Rebind<HasRebind<int>, long>,
		      long*>::value,
	      "nested alias template is used");

template<typename T> struct NoRebind0 { };

static_assert(is_same<Rebind<NoRebind0<int>, long>,
		      NoRebind0<long>>::value,
	      "first template argument is replaced");

template<typename T, typename> struct NoRebind1 { };

static_assert(is_same<Rebind<NoRebind1<int, void>, long>,
		      NoRebind1<long, void>>::value,
	      "first template argument is replaced");

template<typename T, typename, typename> struct NoRebind2 { };

static_assert(is_same<Rebind<NoRebind2<int, void, void>, long>,
		      NoRebind2<long, void, void>>::value,
	      "first template argument is replaced");

template<typename T, typename...> struct NoRebindN { };

static_assert(is_same<Rebind<NoRebindN<int>, long>,
		      NoRebindN<long>>::value,
	      "first template argument is replaced");
static_assert(is_same<Rebind<NoRebindN<int, void>, long>,
		      NoRebindN<long, void>>::value,
	      "first template argument is replaced");

template<typename T, int = 0>
  struct CannotRebind {
    using element_type = T;
  };
// PR libstdc++/72793 specialization of pointer_traits is still well-formed:
std::pointer_traits<CannotRebind<int>>::element_type e;
