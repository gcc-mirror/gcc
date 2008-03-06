// { dg-do compile }

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// Tuple

#include <tr1/tuple>

// libstdc++/35480
void test01()
{
  std::tr1::tuple<int> t1( 1 );
  std::tr1::tuple<int, int> t2( 1, 2 );
  if ( t1 < t2 ) {}   // { dg-error "here" }
  if ( t1 == t2 ) {}  // { dg-error "here" }
}
// { dg-excess-errors "incomplete type" }
