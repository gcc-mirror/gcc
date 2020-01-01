// Copyright (C) 2016-2020 Free Software Foundation, Inc.
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

#include <map>

using mtype1 = std::map<int, int>;
static_assert(std::is_nothrow_default_constructible<mtype1>::value, "Error");

struct cmp
{
  cmp() { }
  bool operator()(int, int) const;
};

using mtype2 = std::map<int, int, cmp>;
static_assert( !std::is_nothrow_default_constructible<mtype2>::value, "Error");

template<typename _Tp>
  struct not_noexcept_cons_alloc : std::allocator<_Tp>
  {
    not_noexcept_cons_alloc() /* noexcept */
    { }

    template<typename _Tp1>
      struct rebind
      { typedef not_noexcept_cons_alloc<_Tp1> other; };
  };

using mtype3 = std::map<int, int, std::less<int>,
			not_noexcept_cons_alloc<std::pair<const int, int>>>;

static_assert(!std::is_nothrow_default_constructible<mtype3>::value, "Error");
