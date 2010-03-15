// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-error "no matching" "" { target *-*-* } 1201 }
// { dg-excess-errors "" }

// Copyright (C) 2009, 2010 Free Software Foundation
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

#include <forward_list>
#include <utility>

void f()
{
  typedef std::forward_list<std::forward_list<std::pair<char, char> > > test_type;
  test_type l('a', 'b');
}
