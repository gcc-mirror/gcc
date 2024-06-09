// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

#include <tuple>

class Empty { };

using std::tuple;
using char_pair = tuple<char, char>;

static_assert( sizeof(tuple<Empty, char_pair>) == sizeof(char_pair),
               "Nested tuple tuple<Empty, tuple<T,T>> is too big");

static_assert( sizeof(tuple<char_pair, char_pair>) == (2 * sizeof(char_pair)),
               "Nested tuple<tuple<T,T, tuple<T,T>> is too big" );
