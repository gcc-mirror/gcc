// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// 25.2.4 replace_if

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::forward_iterator_wrapper;

struct X { };

bool
pred_fn(const X&)
{ return true; }

struct pred_obj
{
  bool
  operator()(const X&)
  { return true; }
};


void
test1(forward_iterator_wrapper<X>& begin,
      forward_iterator_wrapper<X>& end, const X& new_val)
{ return std::replace_if(begin, end, pred_fn, new_val); }

void
test2(forward_iterator_wrapper<X>& begin,
      forward_iterator_wrapper<X>& end, const X& new_val)
{ return std::replace_if(begin, end, pred_obj(), new_val); }


