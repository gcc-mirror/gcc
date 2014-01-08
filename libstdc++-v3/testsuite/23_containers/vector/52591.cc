// Copyright (C) 2012-2014 Free Software Foundation, Inc.
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

// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// libstdc++/52591

#include <vector>

// As an extension we allow move-assignment of std::vector when the element
// type is not MoveAssignable, as long as the allocator type propagates or
// is known to always compare equal.

struct C
{
    C& operator=(C&&) = delete;
};

void test01()
{
    std::vector<C> a;
    a = std::vector<C>();
}
