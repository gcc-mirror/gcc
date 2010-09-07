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
// { dg-error "deleted function" "" { target *-*-* } 233 }
// { dg-error "deleted function" "" { target *-*-* } 251 }
// { dg-error "deleted function" "" { target *-*-* } 269 }
// { dg-error "deleted function" "" { target *-*-* } 287 }
// { dg-error "deleted function" "" { target *-*-* } 305 }
// { dg-error "deleted function" "" { target *-*-* } 323 }
// { dg-error "deleted function" "" { target *-*-* } 341 }
// { dg-error "deleted function" "" { target *-*-* } 359 }
// { dg-error "deleted function" "" { target *-*-* } 377 }
// { dg-error "deleted function" "" { target *-*-* } 395 }
// { dg-error "deleted function" "" { target *-*-* } 413 }
// { dg-error "deleted function" "" { target *-*-* } 431 }
// { dg-error "deleted function" "" { target *-*-* } 449 }
// { dg-error "deleted function" "" { target *-*-* } 467 }
// { dg-error "deleted function" "" { target *-*-* } 485 }
// { dg-excess-errors "In member function" }
