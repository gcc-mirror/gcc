// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <vector>

// C++20 [iterator.traits]: The type iterator_traits<I>::pointer shall be void
// for an iterator of class type I that does not support operator->.
template<typename I>
  concept arrow_or_no_pointer = requires (I i) { i.operator->(); }
    || std::same_as<typename std::iterator_traits<I>::pointer, void>;

static_assert( arrow_or_no_pointer<std::vector<bool>::iterator> );
static_assert( arrow_or_no_pointer<std::vector<bool>::const_iterator> );
