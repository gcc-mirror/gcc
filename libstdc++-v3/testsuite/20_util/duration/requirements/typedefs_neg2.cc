// { dg-do compile { target c++11 } }
// 2008-07-31 Chris Fairles <chris.fairles@gmail.com>

// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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


#include <chrono>

void test01()
{
  // Check if period is a ratio
  typedef int rep_type;
  typedef int period_type;
  typedef std::chrono::duration<rep_type, period_type> test_type;
  test_type d;			// { dg-error "required from here" }
}

// { dg-error "must be a specialization of ratio" "" { target *-*-* } 0 }
// { dg-prune-output "'num' is not a member of 'int'" }
// { dg-prune-output "'den' is not a member of 'int'" }
// { dg-prune-output "'int' is not a class, struct, or union type" }
