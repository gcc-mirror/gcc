// Copyright (C) 2012-2017 Free Software Foundation, Inc.
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

decimal64
__attribute__ ((noinline))
my_nan64 (void)
{
  decimal64 z = 0;
  decimal64 v = z/z;
  return v;
}

decimal64
__attribute__ ((noinline))
my_inf64 (void)
{
  decimal64 o = 1;
  decimal64 z = 0;
  decimal64 v = o/z;
  return v;
}

int
main ()
{
  decimal64 v;

  v = my_nan64 ();

  VERIFY (__builtin_isnand64 (v.__getval ()));
  VERIFY (!__builtin_signbitd64 (v.__getval ()));

  v = -v;

  VERIFY (__builtin_isnand64 (v.__getval ()));
  VERIFY (__builtin_signbitd64 (v.__getval ()));

  v = my_inf64 ();

  VERIFY (__builtin_isinfd64 (v.__getval ()));
  VERIFY (!__builtin_signbitd64 (v.__getval ()));

  v = -v;

  VERIFY (__builtin_isinfd64 (v.__getval ()));
  VERIFY (__builtin_signbitd64 (v.__getval ()));

  return 0;
}
