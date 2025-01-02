// { dg-do compile { target c++14 } }

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

// 8.2.1 Class template shared_ptr [memory.smartptr.shared]

#include <experimental/memory>

using namespace std::experimental;

static_assert( !is_constructible_v<shared_ptr<int>, void*>,
    "can query constructibility without forming invalid type void[]");
static_assert( !is_constructible_v<shared_ptr<int[2]>, void*>,
    "can query constructibility without forming invalid type void[]");
static_assert( !is_constructible_v<shared_ptr<int[]>, void*>,
    "can query constructibility without forming invalid type void[]");

static_assert( !is_constructible_v<shared_ptr<int>, void()>,
    "can query constructibility without forming invalid type void()[]");
static_assert( !is_constructible_v<shared_ptr<int[2]>, void()>,
    "can query constructibility without forming invalid type void()[]");
static_assert( !is_constructible_v<shared_ptr<int[]>, void()>,
    "can query constructibility without forming invalid type void()[]");

static_assert( !is_constructible_v<shared_ptr<int>, void()>,
    "can query constructibility without forming invalid type void(*)()[]");
static_assert( !is_constructible_v<shared_ptr<int[2]>, void()>,
    "can query constructibility without forming invalid type void(*)()[]");
static_assert( !is_constructible_v<shared_ptr<int[]>, void()>,
    "can query constructibility without forming invalid type void(*)()[]");

using A = int[];
static_assert( !is_constructible_v<shared_ptr<int>, A*>,
    "can query constructibility without forming invalid type int[][]");
static_assert( !is_constructible_v<shared_ptr<int[2]>, A*>,
    "can query constructibility without forming invalid type int[][]");
static_assert( !is_constructible_v<shared_ptr<int[]>, A*>,
    "can query constructibility without forming invalid type int[][]");
