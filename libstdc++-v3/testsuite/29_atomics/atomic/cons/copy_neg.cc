// { dg-options "-std=gnu++0x" }
// { dg-do compile }

// Copyright (C) 2008, 2009, 2010 Free Software Foundation, Inc.
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

#include <atomic>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::copy_constructible test;
  __gnu_cxx::typelist::apply_generator(test, __gnu_test::atomics_tl());
  return 0;
}

// { dg-error "used here" "" { target *-*-* } 560 }
// { dg-error "deleted function" "" { target *-*-* } 232 }
// { dg-error "deleted function" "" { target *-*-* } 250 }
// { dg-error "deleted function" "" { target *-*-* } 268 }
// { dg-error "deleted function" "" { target *-*-* } 286 }
// { dg-error "deleted function" "" { target *-*-* } 304 }
// { dg-error "deleted function" "" { target *-*-* } 322 }
// { dg-error "deleted function" "" { target *-*-* } 340 }
// { dg-error "deleted function" "" { target *-*-* } 358 }
// { dg-error "deleted function" "" { target *-*-* } 376 }
// { dg-error "deleted function" "" { target *-*-* } 394 }
// { dg-error "deleted function" "" { target *-*-* } 412 }
// { dg-error "deleted function" "" { target *-*-* } 430 }
// { dg-error "deleted function" "" { target *-*-* } 448 }
// { dg-error "deleted function" "" { target *-*-* } 466 }
// { dg-error "deleted function" "" { target *-*-* } 484 }
// { dg-excess-errors "In member function" }
