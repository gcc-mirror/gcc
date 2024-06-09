// Copyright (C) 2017-2024 Free Software Foundation, Inc.
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
  using Rebind = typename std::allocator_traits<T>::template rebind_alloc<U>;

template<typename T, typename = T>
  struct HasRebind {
    using value_type = T;
    template<typename U> struct rebind { using other = HasRebind<U>; };
  };

// Would get HasRebind<long, int> here if the first template argument is
// replaced instead of using the nested rebind.
static_assert(is_same<Rebind<HasRebind<int>, long>, HasRebind<long>>::value,
	      "nested alias template is used");

template<typename T>
  struct NoRebind0 {
    using value_type = T;
  };

static_assert(is_same<Rebind<NoRebind0<int>, long>,
		      NoRebind0<long>>::value,
	      "first template argument is replaced");

template<typename T, typename>
  struct NoRebind1 {
    using value_type = T;
  };

static_assert(is_same<Rebind<NoRebind1<int, void>, long>,
		      NoRebind1<long, void>>::value,
	      "first template argument is replaced");

template<typename T, typename, typename>
  struct NoRebind2 {
    using value_type = T;
  };

static_assert(is_same<Rebind<NoRebind2<int, void, void>, long>,
		      NoRebind2<long, void, void>>::value,
	      "first template argument is replaced");

template<typename T, typename...>
  struct NoRebindN {
    using value_type = T;
  };

static_assert(is_same<Rebind<NoRebindN<int>, long>,
		      NoRebindN<long>>::value,
	      "first template argument is replaced");
static_assert(is_same<Rebind<NoRebindN<int, void>, long>,
		      NoRebindN<long, void>>::value,
	      "first template argument is replaced");

template<typename T, int = 0>
  struct CannotRebind {
    using value_type = T;
  };
// PR libstdc++/72792 specialization of allocator_traits is still well-formed:
std::allocator_traits<CannotRebind<int>>::value_type v;
