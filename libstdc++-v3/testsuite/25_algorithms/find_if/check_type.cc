// Copyright (C) 2005-2018 Free Software Foundation, Inc.
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

// 25.1.2 find_if

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

struct X { };

using __gnu_test::input_iterator_wrapper;

bool
pred_function(const X&)
{ return true; }

struct pred_obj
{
  bool 
  operator()(const X&)
  { return true; }
};

input_iterator_wrapper<X>
test1(input_iterator_wrapper<X>& begin,
      input_iterator_wrapper<X>& end)
{ return std::find_if(begin, end, pred_function); }

input_iterator_wrapper<X>
test2(input_iterator_wrapper<X>& begin,
      input_iterator_wrapper<X>& end)
{ return std::find_if(begin, end, pred_obj()); }
