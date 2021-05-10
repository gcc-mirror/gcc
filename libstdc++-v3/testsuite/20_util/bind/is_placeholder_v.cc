// { dg-do compile { target c++17 } }

// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#include <functional>

struct X
{
  int operator()() const { return 0; }
  int operator()() volatile { return 1; }
  int operator()() const volatile { return 2; }
  void operator()() { };
};

static_assert( std::is_placeholder<decltype(std::placeholders::_1)>::value
	       == std::is_placeholder_v<decltype(std::placeholders::_1)>);

const auto b0 = std::bind(X());
static_assert( std::is_bind_expression<decltype(b0)>::value
	       == std::is_bind_expression_v<decltype(b0)>);

