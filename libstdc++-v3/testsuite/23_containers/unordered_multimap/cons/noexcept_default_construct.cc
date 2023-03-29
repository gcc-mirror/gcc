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

#include <unordered_map>

using type1 = std::unordered_multimap<int, int>;

static_assert(std::is_nothrow_default_constructible<type1>::value,
	      "noexcept default constructible");

struct not_noexcept_dflt_cons_hash
{
  not_noexcept_dflt_cons_hash() /* noexcept */;

  std::size_t
  operator()(int) const noexcept;
};

using type2 = std::unordered_multimap<int, int, not_noexcept_dflt_cons_hash>;

static_assert( !std::is_nothrow_default_constructible<type2>::value,
	       "not noexcept default constructible");

struct not_noexcept_dflt_cons_equal_to
{
  not_noexcept_dflt_cons_equal_to() /* noexcept */;

  bool
  operator()(int, int) const noexcept;
};

using type3 = std::unordered_multimap<int, int, std::hash<int>,
				       not_noexcept_dflt_cons_equal_to>;

static_assert( !std::is_nothrow_default_constructible<type3>::value,
	       "not noexcept default constructible");

template<typename _Tp>
  struct not_noexcept_dflt_cons_alloc : std::allocator<_Tp>
  {
    not_noexcept_dflt_cons_alloc() /* noexcept */;

    template<typename _Tp1>
      struct rebind
      { typedef not_noexcept_dflt_cons_alloc<_Tp1> other; };
  };

using type4 = std::unordered_multimap<int, int, std::hash<int>, std::equal_to<int>,
				       not_noexcept_dflt_cons_alloc<std::pair<const int, int>>>;

static_assert(!std::is_nothrow_default_constructible<type4>::value,
	      "not noexcept default constructible");
