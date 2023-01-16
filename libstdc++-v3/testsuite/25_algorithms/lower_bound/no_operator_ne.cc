// { dg-do compile }
// Copyright (C) 2000-2023 Free Software Foundation, Inc.
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

#include <algorithm>
#include <testsuite_hooks.h>

// http://gcc.gnu.org/ml/libstdc++/2000-06/msg00316.html
struct foo { };

bool operator== (const foo&, const foo&) { return true; }
bool operator< (const foo&, const foo&) { return true; }

void bar(foo* a, foo* b, foo& x)
{
  foo* c __attribute__((unused)) = std::lower_bound(a, b, x);
}
