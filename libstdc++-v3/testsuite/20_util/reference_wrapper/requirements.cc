// Copyright (C) 2014-2023 Free Software Foundation, Inc.
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

struct NonTrivial
{
  ~NonTrivial() { }
};

using R = std::reference_wrapper<NonTrivial>;

static_assert(std::is_copy_constructible<R>::value, "copy constructible");
static_assert(std::is_copy_assignable<R>::value, "copy assignable");
static_assert(std::is_trivially_copyable<R>::value, "trivially copyable");
