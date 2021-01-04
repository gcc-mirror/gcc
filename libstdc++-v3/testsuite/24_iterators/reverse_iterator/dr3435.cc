// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <iterator>
#include <compare>

// LWG 3435.
// three_way_comparable_with<reverse_iterator<int*>, reverse_iterator<const int*>>

using RI = std::reverse_iterator<int*>;
using CRI = std::reverse_iterator<const int*>;

static_assert( std::three_way_comparable_with<RI, CRI> );

static_assert( std::is_convertible_v<const RI&, CRI> );
static_assert( ! std::is_convertible_v<const CRI&, RI> );

static_assert( std::is_assignable_v<CRI, const RI&> );
static_assert( ! std::is_assignable_v<RI, const CRI&> );
