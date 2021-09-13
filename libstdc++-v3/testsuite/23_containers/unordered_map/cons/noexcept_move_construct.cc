// { dg-do compile { target c++11 } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <unordered_map>

using type1 = std::unordered_map<int, int>;

static_assert( std::is_nothrow_move_constructible<type1>::value,
	       "noexcept move constructor" );
static_assert( std::is_nothrow_constructible<type1,
	       type1&&, const typename type1::allocator_type&>::value,
	       "noexcept move constructor with allocator" );

struct not_noexcept_copy_cons_hash
{
  not_noexcept_copy_cons_hash() noexcept;
  not_noexcept_copy_cons_hash(const not_noexcept_copy_cons_hash&) /* noexcept */;
  not_noexcept_copy_cons_hash(not_noexcept_copy_cons_hash&&) noexcept;

  std::size_t
  operator()(int) const noexcept;
};

using type2 = std::unordered_map<int, int, not_noexcept_copy_cons_hash>;

static_assert( !std::is_nothrow_move_constructible<type2>::value,
	       "not noexcept move constructor" );
static_assert( !std::is_nothrow_constructible<type2, type2&&,
	       const typename type2::allocator_type&>::value,
	       "not noexcept move constructor with allocator" );

struct not_noexcept_copy_cons_equal_to
{
  not_noexcept_copy_cons_equal_to() noexcept;
  not_noexcept_copy_cons_equal_to(const not_noexcept_copy_cons_equal_to&) /* noexcept */;
  not_noexcept_copy_cons_equal_to(not_noexcept_copy_cons_equal_to&&) noexcept;

  bool
  operator()(int, int) const noexcept;
};

using type3 = std::unordered_map<int, int, std::hash<int>,
				  not_noexcept_copy_cons_equal_to>;

static_assert( !std::is_nothrow_move_constructible<type3>::value,
	       "not noexcept move constructor" );
static_assert( !std::is_nothrow_constructible<type3, type3&&,
	       const typename type3::allocator_type&>::value,
	       "not noexcept move constructor with allocator" );
