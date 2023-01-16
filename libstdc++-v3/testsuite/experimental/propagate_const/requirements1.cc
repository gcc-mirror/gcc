// { dg-do compile { target c++14 } }

// Copyright (C) 2013-2023 Free Software Foundation, Inc.
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

#include <experimental/propagate_const>
#include <memory>

using std::experimental::propagate_const;
using std::unique_ptr;
using std::shared_ptr;

propagate_const<int*> test1;
propagate_const<const int*> test2;
propagate_const<unique_ptr<int>> test3;
propagate_const<unique_ptr<const int>> test4;
propagate_const<const unique_ptr<int>> test5;
propagate_const<const unique_ptr<const int>> test6;
propagate_const<shared_ptr<int>> test7;
propagate_const<shared_ptr<const int>> test8;
propagate_const<const shared_ptr<int>> test9;
propagate_const<const shared_ptr<const int>> test10;
