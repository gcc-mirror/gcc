// Copyright (C) 2016-2025 Free Software Foundation, Inc.
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

#include <functional>

struct abstract {
  virtual ~abstract() = 0;
  void operator()() noexcept;
};

static_assert( noexcept(std::__invoke(std::declval<abstract>())),
    "It should be possible to use abstract types with INVOKE" );

struct F {
  void operator()() &;
  void operator()() && noexcept;
  int operator()(int);
  double* operator()(int, int) noexcept;
};
struct D { D(void*); };

static_assert( !noexcept(std::__invoke(std::declval<F&>())), "" );
static_assert( noexcept(std::__invoke(std::declval<F>())), "" );
static_assert( !noexcept(std::__invoke(std::declval<F>(), 1)), "" );
static_assert( noexcept(std::__invoke(std::declval<F>(), 1, 2)), "" );
