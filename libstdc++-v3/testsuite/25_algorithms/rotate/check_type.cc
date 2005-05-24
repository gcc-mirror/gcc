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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 25.2.10 Rotate

// { dg-do compile }

#include <algorithm>
#include <testsuite_iterators.h>

struct X { };

bool operator<(X,X) { return true;}

void
test1(__gnu_test::forward_iterator_wrapper<X>& begin,
      __gnu_test::forward_iterator_wrapper<X>& middle,
      __gnu_test::forward_iterator_wrapper<X>& end)
{ return std::rotate(begin,middle,end); }

void
test1(__gnu_test::bidirectional_iterator_wrapper<X>& begin,
      __gnu_test::bidirectional_iterator_wrapper<X>& middle,
      __gnu_test::bidirectional_iterator_wrapper<X>& end)
{ return std::rotate(begin,middle,end); }

void
test1(__gnu_test::random_access_iterator_wrapper<X>& begin,
      __gnu_test::random_access_iterator_wrapper<X>& middle,
      __gnu_test::random_access_iterator_wrapper<X>& end)
{ return std::rotate(begin,middle,end); }


