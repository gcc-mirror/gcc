// { dg-options "-std=gnu++0x" }
// { dg-do compile }
// 2007-08-22 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2007, 2008 Free Software Foundation, Inc.
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

#include <system_error>
#include <testsuite_error.h>

int main()
{
  bool test __attribute__((unused)) = true;

  __gnu_test::test_category c1;
  __gnu_test::test_category c2(c1); 

  return 0;
}

// { dg-error "is private" "" { target *-*-* } 105 }
// { dg-error "within this context" "" { target *-*-* } 41 }
// { dg-error "first required here" "" { target *-*-* } 31 }
// { dg-excess-errors "copy constructor" }
