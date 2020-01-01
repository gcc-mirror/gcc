// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

// Jonathan Wakely  <jwakely@redhat.com>

// { dg-do compile { target c++11 } }

#include <unordered_map>

using test_type = std::unordered_map<int, int>;
using hasher_type = test_type::hasher;
using alloc_type = test_type::allocator_type;

test_type h1(10, alloc_type());
test_type h2(10, hasher_type(), alloc_type());
test_type h3(h1.begin(), h1.end(), 10, alloc_type());
test_type h4(h1.begin(), h1.end(), 10, hasher_type(), alloc_type());
test_type h5({ { 1, 1 } }, 10, alloc_type());
test_type h6({ { 1, 1 } }, 10, hasher_type(), alloc_type());
