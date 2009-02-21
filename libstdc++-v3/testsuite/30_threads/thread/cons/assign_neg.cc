// { dg-do compile }
// { dg-options "-std=gnu++0x" }
// { dg-require-cstdint "" }
// { dg-require-gthreads "" }

// Copyright (C) 2009 Free Software Foundation, Inc.
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

#include <thread>

void test01()
{
  // assign
  typedef std::thread test_type;
  test_type t1;
  test_type t2;
  t1 = t2;
}

// { dg-error "used here" "" { target *-*-* } 32 }
// { dg-error "deleted function" "" { target *-*-* } 146 }
