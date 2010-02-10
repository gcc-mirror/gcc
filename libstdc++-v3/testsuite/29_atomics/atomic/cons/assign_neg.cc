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
  __gnu_test::assignable test;
  __gnu_cxx::typelist::apply_generator(test, __gnu_test::atomics_tl());
  return 0;
}

// { dg-error "used here" "" { target *-*-* } 521 }
// { dg-error "deleted function" "" { target *-*-* } 230 }
// { dg-error "deleted function" "" { target *-*-* } 248 }
// { dg-error "deleted function" "" { target *-*-* } 266 }
// { dg-error "deleted function" "" { target *-*-* } 284 }
// { dg-error "deleted function" "" { target *-*-* } 302 }
// { dg-error "deleted function" "" { target *-*-* } 320 }
// { dg-error "deleted function" "" { target *-*-* } 338 }
// { dg-error "deleted function" "" { target *-*-* } 356 }
// { dg-error "deleted function" "" { target *-*-* } 374 }
// { dg-error "deleted function" "" { target *-*-* } 392 }
// { dg-error "deleted function" "" { target *-*-* } 410 }
// { dg-error "deleted function" "" { target *-*-* } 428 }
// { dg-error "deleted function" "" { target *-*-* } 446 }
// { dg-error "deleted function" "" { target *-*-* } 464 }
// { dg-error "deleted function" "" { target *-*-* } 482 }
// { dg-excess-errors "In member function" }
