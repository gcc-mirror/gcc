// Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

// { dg-require-effective-target dfp }

#include <decimal/decimal>
#include <testsuite_hooks.h>

using namespace std::decimal;

decimal128
__attribute__ ((noinline))
my_nan128 (void)
{
  decimal128 z = 0;
  decimal128 v = z/z;
  return v;
}

decimal128
__attribute__ ((noinline))
my_inf128 (void)
{
  decimal128 o = 1;
  decimal128 z = 0;
  decimal128 v = o/z;
  return v;
}

int
main ()
{
  decimal128 v;

  v = my_nan128 ();

  VERIFY (__builtin_isnand128 (v.__getval ()));
  VERIFY (!__builtin_signbitd128 (v.__getval ()));

  v = -v;

  VERIFY (__builtin_isnand128 (v.__getval ()));
  VERIFY (__builtin_signbitd128 (v.__getval ()));

  v = my_inf128 ();

  VERIFY (__builtin_isinfd128 (v.__getval ()));
  VERIFY (!__builtin_signbitd128 (v.__getval ()));

  v = -v;

  VERIFY (__builtin_isinfd128 (v.__getval ()));
  VERIFY (__builtin_signbitd128 (v.__getval ()));

  return 0;
}
