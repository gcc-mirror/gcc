// { dg-do compile { target c++11 } }

// 2011-06-01  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

#include <set>

typedef std::multiset<int> mstype;

static_assert( std::is_nothrow_move_constructible<mstype>::value,
	       "noexcept move constructor" );
static_assert( std::is_nothrow_constructible<mstype,
	       mstype&&, const typename mstype::allocator_type&>::value,
	       "noexcept move constructor with allocator" );

template<typename Type>
  class not_noexcept_move_constructor_alloc : public std::allocator<Type>
  {
  public:
    not_noexcept_move_constructor_alloc() noexcept { }

    not_noexcept_move_constructor_alloc(
	const not_noexcept_move_constructor_alloc& x) noexcept
    : std::allocator<Type>(x)
    { }

    not_noexcept_move_constructor_alloc(
	not_noexcept_move_constructor_alloc&& x) noexcept(false)
    : std::allocator<Type>(std::move(x))
    { }

    template<typename _Tp1>
      struct rebind
      { typedef not_noexcept_move_constructor_alloc<_Tp1> other; };
  };

typedef std::multiset<int, std::less<int>,
		 not_noexcept_move_constructor_alloc<int>> amstype;

static_assert( std::is_nothrow_move_constructible<amstype>::value,
	       "noexcept move constructor with not noexcept alloc" );

struct not_noexcept_less
{
  not_noexcept_less() = default;
  not_noexcept_less(const not_noexcept_less&) /* noexcept */
  { }

  bool
  operator()(int l, int r) const
  { return l < r; }
};

typedef std::multiset<int, not_noexcept_less> emstype;

static_assert( !std::is_nothrow_move_constructible<emstype>::value,
	       "not noexcept move constructor with not noexcept less" );

static_assert( !std::is_nothrow_constructible<emstype, emstype&&,
	       const typename emstype::allocator_type&>::value,
	       "not noexcept move constructor with allocator" );
