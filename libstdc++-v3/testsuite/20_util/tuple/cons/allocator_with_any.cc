// { dg-do run { target c++14 } }
// FIXME [!HOSTED]: avoidable std::allocator usage
// { dg-require-effective-target hosted }

// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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


// NOTE: This makes use of the fact that we know how moveable
// is implemented on tuple.  If the implementation changed
// this test may begin to fail.

#include <tuple>
#include <memory>
#include <experimental/any>
#include <testsuite_hooks.h>

using std::experimental::any;

void test01()
{
    std::tuple<any, any> t(std::allocator_arg,
			   std::allocator<any>{});
    VERIFY(std::get<0>(t).empty());
    VERIFY(std::get<1>(t).empty());
}

int main()
{
    test01();
}
