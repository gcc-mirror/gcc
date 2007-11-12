// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2005, 2007 Free Software Foundation, Inc.
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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <deque>
#include <iterator>
#include <testsuite_iterators.h>
#include <testsuite_rvalref.h>

using namespace __gnu_test;
typedef std::deque<rvalstruct> test_type;

// Empty constructor doesn't require a copy constructor
void
test01()
{ test_type d; }

// Constructing from a range that returns rvalue references doesn't
// require a copy constructor.
void
test02(rvalstruct* begin, rvalstruct* end)
{ 
  test_type d(std::make_move_iterator(begin), std::make_move_iterator(end));
}

// Constructing from a input iterator range that returns rvalue
// references doesn't require a copy constructor either.
void
test03(input_iterator_wrapper<rvalstruct> begin,
       input_iterator_wrapper<rvalstruct> end)
{ 
  test_type d(std::make_move_iterator(begin), std::make_move_iterator(end));
}

// Neither does destroying one.
void
test04(test_type* d)
{ delete d; }
