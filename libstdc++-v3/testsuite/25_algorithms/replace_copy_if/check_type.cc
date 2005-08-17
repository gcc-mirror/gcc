// Copyright (C) 2005 Free Software Foundation, Inc.
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

// 25.2.4 replace_copy_if

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::input_iterator_wrapper;
using __gnu_test::output_iterator_wrapper;

struct X { };

struct Y { };

struct Z
{
  Z&
  operator=(const X&)
  { }

  Z&
  operator=(const Y&)
  { }
};

bool
predicate(const X&)
{ return true; }

output_iterator_wrapper<Z>
test1(input_iterator_wrapper<X>& begin,
      input_iterator_wrapper<X>& end,
      output_iterator_wrapper<Z>& output,
      const Y& new_val)
{ 
  return std::replace_copy_if(begin, end, output, predicate, 
			      new_val); 
}
