// { dg-options "-std=gnu++0x" }
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

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <cstdatomic>
#include <testsuite_common_types.h>

int main()
{
  __gnu_test::assignable test;
  __gnu_cxx::typelist::apply_generator(test, __gnu_test::atomics_tl());
  return 0;
}

// { dg-error "used here" "" { target *-*-* } 510 }
// { dg-error "deleted function" "" { target *-*-* } 257 }
// { dg-error "deleted function" "" { target *-*-* } 275 } 
// { dg-error "deleted function" "" { target *-*-* } 293 }
// { dg-error "deleted function" "" { target *-*-* } 311 }
// { dg-error "deleted function" "" { target *-*-* } 329 }
// { dg-error "deleted function" "" { target *-*-* } 347 }
// { dg-error "deleted function" "" { target *-*-* } 365 }
// { dg-error "deleted function" "" { target *-*-* } 383 }
// { dg-error "deleted function" "" { target *-*-* } 401 }
// { dg-error "deleted function" "" { target *-*-* } 419 }
// { dg-error "deleted function" "" { target *-*-* } 437 }
// { dg-error "deleted function" "" { target *-*-* } 455 }
// { dg-error "deleted function" "" { target *-*-* } 473 }
// { dg-error "deleted function" "" { target *-*-* } 491 }
// { dg-error "deleted function" "" { target *-*-* } 239 }
// { dg-excess-errors "In member function" }
