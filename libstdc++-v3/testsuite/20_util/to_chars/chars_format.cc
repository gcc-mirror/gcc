// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++17 } }

#include <charconv>

// C++17 23.2.1 [utility.syn]
// chars_format is a bitmask type with elements scientific, fixed and hex

using F = std::chars_format;
const F none = F{};
const F all = ~none;
static_assert(std::is_enum_v<F>);
static_assert((F::scientific & F::fixed) == none);
static_assert((F::scientific & F::hex) == none);
static_assert((F::fixed & F::hex) == none);
static_assert(F::general == (F::fixed | F::scientific));
static_assert(F::general == (F::fixed ^ F::scientific));

// sanity check operators
static_assert((F::scientific & F::scientific) == F::scientific);
static_assert((F::fixed & F::fixed) == F::fixed);
static_assert((F::hex & F::hex) == F::hex);
static_assert((F::general & F::general) == F::general);
static_assert((F::scientific | F::scientific) == F::scientific);
static_assert((F::fixed | F::fixed) == F::fixed);
static_assert((F::hex | F::hex) == F::hex);
static_assert((F::general | F::general) == F::general);
static_assert((F::scientific ^ F::scientific) == none);
static_assert((F::fixed ^ F::fixed) == none);
static_assert((F::hex ^ F::hex) == none);
static_assert((F::general ^ F::general) == none);
static_assert((F::fixed & all) == F::fixed);
static_assert((F::hex & all) == F::hex);
static_assert((F::general & all) == F::general);
static_assert(~all == none);
