// 2004-09-23 Chris Jefferson <chris@bubblescope.net>

// Copyright (C) 2004 Free Software Foundation, Inc.
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

// Tuple

#include <tr1/tuple>
#include <testsuite_hooks.h>

using namespace std;
using namespace tr1;

int
main()
{
  VERIFY(tuple_size<tuple<> >::value == 0);
  VERIFY(tuple_size<tuple<int> >::value == 1);
  VERIFY(tuple_size<tuple<void> >::value == 1);
  typedef tuple<int,const int&,void> test_tuple1;
  VERIFY(tuple_size<test_tuple1>::value == 3);
  VERIFY(tuple_size<tuple<tuple<void> > >::value == 1);
}

