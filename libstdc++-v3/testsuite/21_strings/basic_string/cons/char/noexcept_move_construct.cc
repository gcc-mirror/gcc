// { dg-do compile { target c++11 } }

// 2011-06-01  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2011-2018 Free Software Foundation, Inc.
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

#include <string>

typedef std::string stype;

// True except for COW strings with _GLIBCXX_FULLY_DYNAMIC_STRING:
static_assert(std::is_nothrow_move_constructible<stype>::value, "Error");

// True for std::allocator because is_always_equal, but not true in general:
static_assert(std::is_nothrow_move_assignable<stype>::value, "lwg 2063");
