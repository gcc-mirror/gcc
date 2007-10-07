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

// 25.2.9 Reverse

#undef _GLIBCXX_CONCEPT_CHECKS

#include <algorithm>
#include <testsuite_iterators.h>

using __gnu_test::bidirectional_iterator_wrapper;

class X 
{ 
  X();
  X(const X&);
  void operator=(const X&);
};

void
swap(X&, X&) { }

void
test1(bidirectional_iterator_wrapper<X>& begin, 
      bidirectional_iterator_wrapper<X>& end)
{ std::reverse(begin, end); }
